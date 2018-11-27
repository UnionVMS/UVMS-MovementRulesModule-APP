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
import java.util.Date;    //Leave be for now
import java.util.List;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.Queue;
import javax.jms.TextMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.exchange.module.v1.ExchangeModuleMethod;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.RecipientInfoType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.EmailType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.PluginType;
import eu.europa.ec.fisheries.schema.exchange.service.v1.ServiceResponseType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.exchange.model.exception.ExchangeModelMapperException;
import eu.europa.ec.fisheries.uvms.exchange.model.mapper.ExchangeDataSourceResponseMapper;
import eu.europa.ec.fisheries.uvms.exchange.model.mapper.ExchangeModuleRequestMapper;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.consumer.RulesResponseConsumer;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.ExchangeProducerBean;

@Stateless
public class ExchangeServiceBean {

    private static final Logger LOG = LoggerFactory.getLogger(ExchangeServiceBean.class);
    
    @Inject
    private RulesResponseConsumer consumer;

    @Inject
    private ExchangeProducerBean exchangeProducer;

    @Resource(mappedName = "java:/" + MessageConstants.QUEUE_MOVEMENTRULES)
    private Queue responseQueue;

    public List<ServiceResponseType> getPluginList(PluginType pluginType) throws ExchangeModelMapperException, MessageException {
        ArrayList<PluginType> types = new ArrayList<>();
        types.add(pluginType);
        String serviceListRequest = ExchangeModuleRequestMapper.createGetServiceListRequest(types);
        String serviceListRequestId = exchangeProducer.sendModuleMessage(serviceListRequest, responseQueue, ExchangeModuleMethod.LIST_SERVICES.value());

        TextMessage serviceListResponse = consumer.getMessage(serviceListRequestId, TextMessage.class);
        return ExchangeDataSourceResponseMapper.mapToServiceTypeListFromModuleResponse(serviceListResponse, serviceListRequestId);
    }
    
    public void sendReportToPlugin(PluginType pluginType, String ruleName, String endpoint, MovementType exchangeMovement, List<RecipientInfoType> recipientInfoList, MovementDetails movementDetails) throws ExchangeModelMapperException, MessageException {
        String exchangeRequest = ExchangeModuleRequestMapper.createSendReportToPlugin("", pluginType, new Date(), ruleName, endpoint, exchangeMovement, recipientInfoList, movementDetails.getAssetName(), movementDetails.getIrcs(), movementDetails.getMmsi(), movementDetails.getExternalMarking(), movementDetails.getFlagState());
        exchangeProducer.sendModuleMessage(exchangeRequest, responseQueue, ExchangeModuleMethod.SEND_REPORT_TO_PLUGIN.value());
    }
    
    public void sendEmail(ServiceResponseType service, EmailType email, String ruleName) throws ExchangeModelMapperException, MessageException {
        String request = ExchangeModuleRequestMapper.createSetCommandSendEmailRequest(service.getServiceClassName(), email, ruleName);
        exchangeProducer.sendModuleMessage(request, responseQueue, ExchangeModuleMethod.SEND_REPORT_TO_PLUGIN.value());
    }
}
