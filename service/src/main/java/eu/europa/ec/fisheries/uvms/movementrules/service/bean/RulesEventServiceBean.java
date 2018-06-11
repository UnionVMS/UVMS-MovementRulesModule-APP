/*
﻿Developed with the contribution of the European Commission - Directorate General for Maritime Affairs and Fisheries
© European Union, 2015-2016.

This file is part of the Integrated Fisheries Data Management (IFDM) Suite. The IFDM Suite is free software: you can
redistribute it and/or modify it under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or any later version. The IFDM Suite is distributed in
the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a
copy of the GNU General Public License along with the IFDM Suite. If not, see <http://www.gnu.org/licenses/>.
 */

package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import javax.ejb.Stateless;
import javax.enterprise.event.Event;
import javax.enterprise.event.Observes;
import javax.inject.Inject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.PingResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.RulesBaseRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.RulesModuleMethod;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.SetMovementReportRequest;
import eu.europa.ec.fisheries.uvms.audit.model.exception.AuditModelMarshallException;
import eu.europa.ec.fisheries.uvms.audit.model.mapper.AuditLogMapper;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.movementrules.message.constants.DataSourceQueue;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.ErrorEvent;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.GetTicketsAndRulesByMovementsEvent;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.PingReceivedEvent;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.SetMovementReportReceivedEvent;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.carrier.EventMessage;
import eu.europa.ec.fisheries.uvms.movementrules.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.uvms.movementrules.model.constant.AuditObjectTypeEnum;
import eu.europa.ec.fisheries.uvms.movementrules.model.constant.AuditOperationEnum;
import eu.europa.ec.fisheries.uvms.movementrules.model.constant.FaultCode;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelMapperException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.ModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.RulesModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.EventService;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.DaoException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.DaoMappingException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;

@Stateless
public class RulesEventServiceBean implements EventService {

    private static final Logger LOG = LoggerFactory.getLogger(RulesEventServiceBean.class);

    @Inject
    @ErrorEvent
    private Event<EventMessage> errorEvent;

    @Inject
    private RulesMessageProducer rulesProducer;

    @Inject
    private RulesService rulesService;
    
    @Inject
    private MovementReportProcessorBean movementReportBean;

    @Override
    public void pingReceived(@Observes @PingReceivedEvent EventMessage eventMessage) {
        try {
            PingResponse pingResponse = new PingResponse();
            pingResponse.setResponse("pong");
            String pingResponseText = JAXBMarshaller.marshallJaxBObjectToString(pingResponse);
            rulesProducer.sendModuleResponseMessage(eventMessage.getJmsMessage(), pingResponseText);
        } catch (RulesModelMarshallException | MessageException e) {
            LOG.error("[ERROR] Error when responding to ping {}", e.getMessage());
            errorEvent.fire(eventMessage);
        }
    }

    @Override
    public void setMovementReportReceived(@Observes @SetMovementReportReceivedEvent EventMessage message) {
        try {
            RulesBaseRequest baseRequest = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), RulesBaseRequest.class);
            if (baseRequest.getMethod() != RulesModuleMethod.SET_MOVEMENT_REPORT) {
                LOG.error("[ERROR] Error, Set Movement Report invoked but it is not the intended method, caller is trying : {}", baseRequest.getMethod().name());
            }
            SetMovementReportRequest request = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), SetMovementReportRequest.class);
            movementReportBean.setMovementReportReceived(request.getRequest(), request.getType().name(), baseRequest.getUsername());
        } catch (RulesModelMapperException | RulesServiceException e) {
            LOG.error("[ERROR] Error when creating movement {}", e.getMessage());
        }
    }

    @Override
    public void getTicketsAndRulesByMovementsEvent(@Observes @GetTicketsAndRulesByMovementsEvent EventMessage message) {
        try {
            RulesBaseRequest baseRequest = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), RulesBaseRequest.class);
            if (baseRequest.getMethod() != RulesModuleMethod.GET_TICKETS_AND_RULES_BY_MOVEMENTS) {
                errorEvent.fire(new EventMessage(message.getJmsMessage(), ModuleResponseMapper.createFaultMessage(FaultCode.RULES_MESSAGE,
                        "[ERROR] Error, Get Tickets And Rules By Movements invoked but it is not the intended method, caller is trying: {}"
                                + baseRequest.getMethod().name())));
            }
            GetTicketsAndRulesByMovementsRequest request = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), GetTicketsAndRulesByMovementsRequest.class);
            GetTicketsAndRulesByMovementsResponse response = rulesService.getTicketsAndRulesByMovements(request.getMovementGuids());
            rulesProducer.sendModuleResponseMessage(message.getJmsMessage(), RulesModuleResponseMapper.getTicketsAndRulesByMovementsResponse(response.getTicketsAndRules()));
        } catch (RulesModelMapperException | RulesServiceException | MessageException | DaoException | DaoMappingException e) {
            LOG.error("[ERROR] Error when fetching tickets and rules by movements {}", e.getMessage());
            errorEvent.fire(message);
        }
    }
}