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
import java.util.List;
import java.util.UUID;
import javax.ejb.ActivationConfigProperty;
import javax.ejb.MessageDriven;
import javax.inject.Inject;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;
import eu.europa.ec.fisheries.schema.mobileterminal.module.v1.MobileTerminalModuleBaseRequest;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ComChannelAttribute;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ComChannelType;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalId;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.MobileTerminalType;
import eu.europa.ec.fisheries.uvms.mobileterminal.model.mapper.MobileTerminalModuleRequestMapper;
import eu.europa.ec.fisheries.uvms.movement.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.movementrules.message.producer.RulesMessageProducer;

@MessageDriven(mappedName = "jms/queue/UVMSMobileTerminalEvent", activationConfig = {
        @ActivationConfigProperty(propertyName = "messagingType", propertyValue = "javax.jms.MessageListener"), 
        @ActivationConfigProperty(propertyName = "destinationType", propertyValue = "javax.jms.Queue"), 
        @ActivationConfigProperty(propertyName = "destination", propertyValue = "UVMSMobileTerminalEvent")})
public class MobileTerminalModuleMock implements MessageListener {
    
    @Inject
    RulesMessageProducer messageProducer;

    @Override
    public void onMessage(Message message) {
        try {
            TextMessage textMessage = (TextMessage) message;
            MobileTerminalModuleBaseRequest request = JAXBMarshaller.unmarshallTextMessage(textMessage, MobileTerminalModuleBaseRequest.class);
            
            switch (request.getMethod()) {
                case LIST_MOBILE_TERMINALS:
                    List<MobileTerminalType> mobileTerminals = new ArrayList<>();
                    MobileTerminalType mobileTerminal = getBasicMobileTerminalType();
                    mobileTerminals.add(mobileTerminal);
                    String responseString = MobileTerminalModuleRequestMapper.mapGetMobileTerminalList(mobileTerminals);
                    messageProducer.sendModuleResponseMessage((TextMessage) message, responseString);
                    break;
                default:
                    break;
            }
            
        } catch (Exception e) {
        }
    }
    
    private static MobileTerminalType getBasicMobileTerminalType() {
        MobileTerminalType mobileTerminal = new MobileTerminalType();
        mobileTerminal.setConnectId(UUID.randomUUID().toString());
        MobileTerminalId mobileTerminalId = new MobileTerminalId();
        mobileTerminalId.setGuid(UUID.randomUUID().toString());
        mobileTerminal.setMobileTerminalId(mobileTerminalId);
        ComChannelType channel = new ComChannelType();
        ComChannelAttribute channelAttribute = new ComChannelAttribute();
        channelAttribute.setType("DNID");
        channelAttribute.setValue("TEST_DNID");
        channel.getAttributes().add(channelAttribute);
        ComChannelAttribute channelAttribute2 = new ComChannelAttribute();
        channelAttribute2.setType("MEMBER_NUMBER");
        channelAttribute2.setValue("TEST_MEMBER_NUMBER");
        channel.getAttributes().add(channelAttribute2);
        mobileTerminal.getChannels().add(channel);
        return mobileTerminal;
    }
}