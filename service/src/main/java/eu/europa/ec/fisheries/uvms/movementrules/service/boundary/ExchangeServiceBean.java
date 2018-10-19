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
package eu.europa.ec.fisheries.uvms.movementrules.service.boundary;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.TextMessage;

import eu.europa.ec.fisheries.schema.exchange.module.v1.ExchangeModuleMethod;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.ExchangeProducerBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementRefType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementRefTypeType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.RecipientInfoType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.SetReportMovementType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.EmailType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.PluginType;
import eu.europa.ec.fisheries.schema.exchange.service.v1.ServiceResponseType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.exchange.model.exception.ExchangeModelMapperException;
import eu.europa.ec.fisheries.uvms.exchange.model.mapper.ExchangeDataSourceResponseMapper;
import eu.europa.ec.fisheries.uvms.exchange.model.mapper.ExchangeModuleRequestMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.constants.DataSourceQueue;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.consumer.RulesResponseConsumer;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MovementFact;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.ExchangeMovementMapper;

@Stateless
public class ExchangeServiceBean {

    private static final Logger LOG = LoggerFactory.getLogger(ExchangeServiceBean.class);
    
    @Inject
    private RulesResponseConsumer consumer;

    @Inject
    private ExchangeProducerBean exchangeProducer;
    
    public void sendBackToExchange(String guid, RawMovementType rawMovement, MovementRefTypeType status, String username) throws MessageException {
        LOG.info("Sending back processed movement ({}) to Exchange", guid);

        // Map response
        MovementRefType movementRef = new MovementRefType();
        movementRef.setMovementRefGuid(guid);
        movementRef.setType(status);
        movementRef.setAckResponseMessageID(rawMovement.getAckResponseMessageID());

        // Map movement
        SetReportMovementType setReportMovementType = ExchangeMovementMapper.mapExchangeMovement(rawMovement);

        try {
            String exchangeResponseText = ExchangeMovementMapper.mapToProcessedMovementResponse(setReportMovementType, movementRef, username);
            exchangeProducer.sendModuleMessage(exchangeResponseText, ExchangeModuleMethod.PROCESSED_MOVEMENT.value());

            //this is here to make rules respond on the test queue as well as to exchange, dont use unless you are running performance tests from docker.
            //producer.sendResponseMessageForTest(exchangeResponseText, username);
        } catch (Exception e) {
            LOG.error("Could not send processed movement to Exchange", e);
        }
    }
    
    public List<ServiceResponseType> getPluginList(PluginType pluginType) throws ExchangeModelMapperException, MessageException {
        ArrayList<PluginType> types = new ArrayList<>();
        types.add(pluginType);
        String serviceListRequest = ExchangeModuleRequestMapper.createGetServiceListRequest(types);
        String serviceListRequestId = exchangeProducer.sendModuleMessage(serviceListRequest, ExchangeModuleMethod.LIST_SERVICES.value());

        TextMessage serviceListResponse = consumer.getMessage(serviceListRequestId, TextMessage.class);
        return ExchangeDataSourceResponseMapper.mapToServiceTypeListFromModuleResponse(serviceListResponse, serviceListRequestId);
    }
    
    public void sendReportToPlugin(ServiceResponseType service, PluginType pluginType, String ruleName, String endpoint, MovementType exchangeMovement, List<RecipientInfoType> recipientInfoList, MovementFact fact) throws ExchangeModelMapperException, MessageException {
        String exchangeRequest = ExchangeModuleRequestMapper.createSendReportToPlugin(service.getServiceClassName(), pluginType, new Date(), ruleName, endpoint, exchangeMovement, recipientInfoList, fact.getAssetName(), fact.getIrcs(), fact.getMmsiNo(), fact.getExternalMarking(), fact.getFlagState());
        String messageId = exchangeProducer.sendModuleMessage(exchangeRequest, ExchangeModuleMethod.SEND_REPORT_TO_PLUGIN.value());
        consumer.getMessage(messageId, TextMessage.class);
    }
    
    public void sendEmail(ServiceResponseType service, EmailType email, String ruleName) throws ExchangeModelMapperException, MessageException {
        String request = ExchangeModuleRequestMapper.createSetCommandSendEmailRequest(service.getServiceClassName(), email, ruleName);
        exchangeProducer.sendModuleMessage(request, ExchangeModuleMethod.SEND_REPORT_TO_PLUGIN.value());
    }
}
