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
import javax.ejb.ActivationConfigProperty;
import javax.ejb.MessageDriven;
import javax.inject.Inject;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.uvms.rules.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.uvms.rules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.user.model.mapper.UserModuleResponseMapper;
import eu.europa.ec.fisheries.wsdl.user.module.UserBaseRequest;
import eu.europa.ec.fisheries.wsdl.user.types.ContactDetails;
import eu.europa.ec.fisheries.wsdl.user.types.ContextSet;
import eu.europa.ec.fisheries.wsdl.user.types.Organisation;
import eu.europa.ec.fisheries.wsdl.user.types.UserContext;

@MessageDriven(mappedName = "jms/queue/UVMSUserEvent", activationConfig = {
        @ActivationConfigProperty(propertyName = "messagingType", propertyValue = "javax.jms.MessageListener"),
        @ActivationConfigProperty(propertyName = "destinationType", propertyValue = "javax.jms.Queue"),
        @ActivationConfigProperty(propertyName = "destination", propertyValue = "UVMSUserEvent")})
public class UserModuleMock implements MessageListener {

    final static Logger LOG = LoggerFactory.getLogger(UserModuleMock.class);
    
    @Inject
    RulesMessageProducer messageProducer;
    
    @Override
    public void onMessage(Message message) {
        TextMessage textMessage = (TextMessage) message;
        try {
            UserBaseRequest request = JAXBMarshaller.unmarshallTextMessage(textMessage, UserBaseRequest.class);
            switch (request.getMethod()) {
                case GET_CONTACT_DETAILS:
                    ContactDetails contactDetails = new ContactDetails();
                    contactDetails.setOrganisationName("Test Organisation");
                    String response = UserModuleResponseMapper.mapToGetContactDetailsResponse(contactDetails);
                    messageProducer.sendModuleResponseMessage((TextMessage) message, response);
                    break;
                case GET_USER_CONTEXT:
                    UserContext userContext = new UserContext();
                    userContext.setContextSet(new ContextSet());
                    String contextResponse =  UserModuleResponseMapper.mapToGetUserContextResponse(userContext);
                    messageProducer.sendModuleResponseMessage((TextMessage) message, contextResponse);
                    break;
                case FIND_ORGANISATIONS:
                    List<Organisation> organizations = new ArrayList<>();
                    String organisationResponse = UserModuleResponseMapper.mapToFindOrganisationsResponse(organizations);
                    messageProducer.sendModuleResponseMessage((TextMessage) message, organisationResponse);
                    break;
                default:
                    break;
            }
        } catch (Exception e) {
            LOG.error("UserModuleMock Error", e);
        }
    }
}
