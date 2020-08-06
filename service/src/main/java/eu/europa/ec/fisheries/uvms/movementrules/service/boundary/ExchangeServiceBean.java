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

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.JMSException;
import javax.jms.TextMessage;
import eu.europa.ec.fisheries.schema.exchange.module.v1.ExchangeModuleMethod;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.MovementType;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.RecipientInfoType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.EmailType;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.PluginType;
import eu.europa.ec.fisheries.schema.exchange.service.v1.ServiceResponseType;
import eu.europa.ec.fisheries.uvms.exchange.model.mapper.ExchangeDataSourceResponseMapper;
import eu.europa.ec.fisheries.uvms.exchange.model.mapper.ExchangeModuleRequestMapper;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.consumer.RulesResponseConsumer;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.ExchangeProducerBean;

@Stateless
public class ExchangeServiceBean {


    @Inject
    private ExchangeProducerBean exchangeProducer;


    public void sendReportToPlugin(String pluginName, String ruleName, String recipient, MovementType exchangeMovement, List<RecipientInfoType> recipientInfoList, MovementDetails movementDetails) throws  JMSException {
        String exchangeRequest = ExchangeModuleRequestMapper.createSendReportToPlugin(pluginName, null, Instant.now(), ruleName, recipient, exchangeMovement, recipientInfoList, movementDetails.getAssetName(), movementDetails.getIrcs(), movementDetails.getMmsi(), movementDetails.getExternalMarking(), movementDetails.getFlagState());
        exchangeProducer.sendModuleMessage(exchangeRequest, ExchangeModuleMethod.SEND_REPORT_TO_PLUGIN.value());
    }
    
}
