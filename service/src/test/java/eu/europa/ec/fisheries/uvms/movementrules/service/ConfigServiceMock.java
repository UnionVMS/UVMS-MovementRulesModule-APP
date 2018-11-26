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
package eu.europa.ec.fisheries.uvms.movementrules.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.ejb.ActivationConfigProperty;
import javax.ejb.MessageDriven;
import javax.inject.Inject;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;
import eu.europa.ec.fisheries.schema.config.module.v1.ConfigModuleBaseRequest;
import eu.europa.ec.fisheries.schema.config.types.v1.PullSettingsStatus;
import eu.europa.ec.fisheries.schema.config.types.v1.SettingType;
import eu.europa.ec.fisheries.uvms.config.model.mapper.ModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.RulesMessageProducerBean;

@MessageDriven(mappedName = "jms/queue/UVMSConfigEvent", activationConfig = {
        @ActivationConfigProperty(propertyName = "messagingType", propertyValue = "javax.jms.MessageListener"),
        @ActivationConfigProperty(propertyName = "destinationType", propertyValue = "javax.jms.Queue"),
        @ActivationConfigProperty(propertyName = "destination", propertyValue = "UVMSConfigEvent")})
public class ConfigServiceMock implements MessageListener {
    
    @Inject
    RulesMessageProducerBean messageProducer;
    
    @Override
    public void onMessage(Message message) {
        TextMessage textMessage = (TextMessage) message;
        try {
            ConfigModuleBaseRequest request = JAXBMarshaller.unmarshallTextMessage(textMessage, ConfigModuleBaseRequest.class);
            switch (request.getMethod()) {
                case PULL:
                    SettingType mockSetting = new SettingType();
                    mockSetting.setKey("flux_local_nation_code");
                    mockSetting.setValue("SWE");
                    mockSetting.setDescription("From ConfigServiceMock.java");
                    String pullResponse = ModuleResponseMapper.toPullSettingsResponse(Arrays.asList(mockSetting), PullSettingsStatus.OK);
                    messageProducer.sendModuleResponseMessage((TextMessage) message, pullResponse);
                    break;
                default:
                    break;
            }
        } catch (Exception e) {
            
        }
    }
}
