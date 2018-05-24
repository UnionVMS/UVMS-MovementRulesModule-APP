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

package eu.europa.ec.fisheries.uvms.rules.service.bean;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.enterprise.event.Event;
import javax.enterprise.event.Observes;
import javax.inject.Inject;

import eu.europa.ec.fisheries.uvms.rules.exception.DaoMappingException;
import eu.europa.ec.fisheries.uvms.rules.mapper.CustomRuleMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.schema.rules.module.v1.CountTicketsByMovementsRequest;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetCustomRuleRequest;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetTicketsAndRulesByMovementsRequest;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetTicketsByMovementsRequest;
import eu.europa.ec.fisheries.schema.rules.module.v1.PingResponse;
import eu.europa.ec.fisheries.schema.rules.module.v1.RulesBaseRequest;
import eu.europa.ec.fisheries.schema.rules.module.v1.RulesModuleMethod;
import eu.europa.ec.fisheries.schema.rules.module.v1.SetMovementReportRequest;
import eu.europa.ec.fisheries.schema.rules.source.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.uvms.audit.model.exception.AuditModelMarshallException;
import eu.europa.ec.fisheries.uvms.audit.model.mapper.AuditLogMapper;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.rules.message.constants.DataSourceQueue;
import eu.europa.ec.fisheries.uvms.rules.message.event.CountTicketsByMovementsEvent;
import eu.europa.ec.fisheries.uvms.rules.message.event.ErrorEvent;
import eu.europa.ec.fisheries.uvms.rules.message.event.GetCustomRuleReceivedEvent;
import eu.europa.ec.fisheries.uvms.rules.message.event.GetTicketsAndRulesByMovementsEvent;
import eu.europa.ec.fisheries.uvms.rules.message.event.GetTicketsByMovementsEvent;
import eu.europa.ec.fisheries.uvms.rules.message.event.PingReceivedEvent;
import eu.europa.ec.fisheries.uvms.rules.message.event.SetMovementReportReceivedEvent;
import eu.europa.ec.fisheries.uvms.rules.message.event.carrier.EventMessage;
import eu.europa.ec.fisheries.uvms.rules.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.uvms.rules.model.constant.AuditObjectTypeEnum;
import eu.europa.ec.fisheries.uvms.rules.model.constant.AuditOperationEnum;
import eu.europa.ec.fisheries.uvms.rules.model.constant.FaultCode;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesFaultException;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesModelMapperException;
import eu.europa.ec.fisheries.uvms.rules.model.exception.RulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.rules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.rules.model.mapper.ModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.rules.model.mapper.RulesModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.rules.service.EventService;
import eu.europa.ec.fisheries.uvms.rules.service.RulesService;
import eu.europa.ec.fisheries.uvms.rules.service.exception.RulesServiceException;

@Stateless
public class RulesEventServiceBean implements EventService {

    private static final Logger LOG = LoggerFactory.getLogger(RulesEventServiceBean.class);

    @Inject
    @ErrorEvent
    private Event<EventMessage> errorEvent;

    @Inject
    private RulesMessageProducer rulesProducer;

    @EJB
    private RulesService rulesService;

    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
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
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public void setMovementReportReceived(@Observes @SetMovementReportReceivedEvent EventMessage message) {
        LOG.info("[INFO] Validating movement from Received from Exchange Module..");
        try {
            RulesBaseRequest baseRequest = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), RulesBaseRequest.class);
            if (baseRequest.getMethod() != RulesModuleMethod.SET_MOVEMENT_REPORT) {
                LOG.error("[ERROR] Error, Set Movement Report invoked but it is not the intended method, caller is trying : " + baseRequest.getMethod().name());
            }
            SetMovementReportRequest request = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), SetMovementReportRequest.class);
            rulesService.setMovementReportReceived(request.getRequest(), request.getType().name(), baseRequest.getUsername());
        } catch (RulesModelMapperException | RulesServiceException e) {
            LOG.error("[ERROR] Error when creating movement {}", e.getMessage());
        }
    }

    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public void getCustomRule(@Observes @GetCustomRuleReceivedEvent EventMessage message) {
        LOG.info("[INFO] Get custom rule by guid..");
        try {
            RulesBaseRequest baseRequest = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), RulesBaseRequest.class);
            if (baseRequest.getMethod() != RulesModuleMethod.GET_CUSTOM_RULE) {
                errorEvent.fire(new EventMessage(message.getJmsMessage(), ModuleResponseMapper.createFaultMessage(FaultCode.RULES_MESSAGE,
                        "[ERROR] Error, Get Custom Rule invoked but it is not the intended method, caller is trying: "
                                + baseRequest.getMethod().name())));
            }
            GetCustomRuleRequest request = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), GetCustomRuleRequest.class);
            CustomRuleType response = CustomRuleMapper.toCustomRuleType(rulesService.getCustomRuleByGuid(request.getGuid()));
            rulesProducer.sendModuleResponseMessage(message.getJmsMessage(), RulesModuleResponseMapper.mapToGetCustomRuleResponse(response));
        } catch (RulesModelMapperException | RulesServiceException | RulesFaultException | MessageException | DaoMappingException e) {
            LOG.error("[ERROR] Error when fetching rule by guid {}", e.getMessage());
            errorEvent.fire(message);
        }
    }

    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public void getTicketsByMovements(@Observes @GetTicketsByMovementsEvent EventMessage message) {
        LOG.info("[INFO] Fetch tickets by movements..");
        try {
            RulesBaseRequest baseRequest = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), RulesBaseRequest.class);
            if (baseRequest.getMethod() != RulesModuleMethod.GET_TICKETS_BY_MOVEMENTS) {
                errorEvent.fire(new EventMessage(message.getJmsMessage(), ModuleResponseMapper.createFaultMessage(FaultCode.RULES_MESSAGE,
                        "[ERROR] Error, Get Tickets By Movements invoked but it is not the intended method, caller is trying: "
                                + baseRequest.getMethod().name())));
            }
            GetTicketsByMovementsRequest request = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), GetTicketsByMovementsRequest.class);
            GetTicketListByMovementsResponse response = rulesService.getTicketsByMovements(request.getMovementGuids());
            String responseString = RulesModuleResponseMapper.mapToGetTicketListByMovementsResponse(response.getTickets());
            rulesProducer.sendModuleResponseMessage(message.getJmsMessage(), responseString);
        } catch (RulesModelMapperException | RulesServiceException | RulesFaultException | MessageException e) {
            LOG.error("[ERROR] Error when fetching tickets by movements {}", e.getMessage());
            errorEvent.fire(message);
        }

    }

    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public void countTicketsByMovementsEvent(@Observes @CountTicketsByMovementsEvent EventMessage message) {
        LOG.info("[INFO] Count tickets by movements..");
        try {
            RulesBaseRequest baseRequest = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), RulesBaseRequest.class);
            if (baseRequest.getMethod() != RulesModuleMethod.COUNT_TICKETS_BY_MOVEMENTS) {
                errorEvent.fire(new EventMessage(message.getJmsMessage(), ModuleResponseMapper.createFaultMessage(FaultCode.RULES_MESSAGE,
                        "[ERROR] Error, count tickets by movements invoked but it is not the intended method, caller is trying: "
                                + baseRequest.getMethod().name())));
            }
            CountTicketsByMovementsRequest request = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), CountTicketsByMovementsRequest.class);
            long response = rulesService.countTicketsByMovements(request.getMovementGuids());
            rulesProducer.sendModuleResponseMessage(message.getJmsMessage(), RulesModuleResponseMapper.mapToCountTicketListByMovementsResponse(response));
        } catch (RulesModelMapperException | RulesServiceException | RulesFaultException | MessageException e) {
            LOG.error("[ERROR] Error when fetching ticket count by movements {}", e.getMessage());
            errorEvent.fire(message);
        }
    }

    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public void getTicketsAndRulesByMovementsEvent(@Observes @GetTicketsAndRulesByMovementsEvent EventMessage message) {
        LOG.info("[INFO] Fetch tickets and rules by movements..");
        try {
            RulesBaseRequest baseRequest = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), RulesBaseRequest.class);
            if (baseRequest.getMethod() != RulesModuleMethod.GET_TICKETS_AND_RULES_BY_MOVEMENTS) {
                errorEvent.fire(new EventMessage(message.getJmsMessage(), ModuleResponseMapper.createFaultMessage(FaultCode.RULES_MESSAGE,
                        "[ERROR] Error, Get Tickets And Rules By Movements invoked but it is not the intended method, caller is trying: "
                                + baseRequest.getMethod().name())));
            }
            GetTicketsAndRulesByMovementsRequest request = JAXBMarshaller.unmarshallTextMessage(message.getJmsMessage(), GetTicketsAndRulesByMovementsRequest.class);
            GetTicketsAndRulesByMovementsResponse response = rulesService.getTicketsAndRulesByMovements(request.getMovementGuids());
            rulesProducer.sendModuleResponseMessage(message.getJmsMessage(), RulesModuleResponseMapper.getTicketsAndRulesByMovementsResponse(response.getTicketsAndRules()));
        } catch (RulesModelMapperException | RulesServiceException | MessageException e) {
            LOG.error("[ERROR] Error when fetching tickets and rules by movements {}", e.getMessage());
            errorEvent.fire(message);
        }
    }


 
    @SuppressWarnings("unused")
	private void sendAuditMessage(AuditObjectTypeEnum type, AuditOperationEnum operation, String affectedObject, String comment, String username) {
        try {
            String message = AuditLogMapper.mapToAuditLog(type.getValue(), operation.getValue(), affectedObject, comment, username);
            rulesProducer.sendDataSourceMessage(message, DataSourceQueue.AUDIT);
        }
        catch (AuditModelMarshallException | MessageException e) {
            LOG.error("[ERROR] Error when sending message to Audit {}", e.getMessage());
        }
    }

}