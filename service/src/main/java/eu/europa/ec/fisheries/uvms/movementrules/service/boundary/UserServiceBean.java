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
import java.util.Iterator;
import java.util.List;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.jms.Queue;
import javax.jms.TextMessage;
import javax.xml.bind.JAXBException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.exchange.movement.v1.RecipientInfoType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.consumer.RulesResponseConsumer;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.UserProducerBean;
import eu.europa.ec.fisheries.uvms.user.model.exception.ModelMarshallException;
import eu.europa.ec.fisheries.uvms.user.model.mapper.UserModuleRequestMapper;
import eu.europa.ec.fisheries.wsdl.user.module.FindOrganisationsResponse;
import eu.europa.ec.fisheries.wsdl.user.module.GetContactDetailResponse;
import eu.europa.ec.fisheries.wsdl.user.module.GetUserContextResponse;
import eu.europa.ec.fisheries.wsdl.user.module.UserModuleMethod;
import eu.europa.ec.fisheries.wsdl.user.types.Channel;
import eu.europa.ec.fisheries.wsdl.user.types.EndPoint;
import eu.europa.ec.fisheries.wsdl.user.types.Organisation;
import eu.europa.ec.fisheries.wsdl.user.types.UserContext;
import eu.europa.ec.fisheries.wsdl.user.types.UserContextId;

@Stateless
public class UserServiceBean {

    private static final Logger LOG = LoggerFactory.getLogger(UserServiceBean.class);
    
    @Inject
    private RulesResponseConsumer consumer;

    @Inject
    private UserProducerBean producer;

    @Resource(mappedName = "java:/" + MessageConstants.QUEUE_MOVEMENTRULES)
    private Queue responseQueue;
    
    public UserContext getFullUserContext(String remoteUser, String applicationName) {
        LOG.debug("Request getFullUserContext({}, {})", remoteUser, applicationName);
        UserContext userContext = null;
        UserContextId contextId = new UserContextId();
        contextId.setApplicationName(applicationName);
        contextId.setUserName(remoteUser);
        String userRequest;
        try {
            userRequest = UserModuleRequestMapper.mapToGetUserContextRequest(contextId);
            String messageId = producer.sendModuleMessage(userRequest, responseQueue, UserModuleMethod.GET_USER_CONTEXT.value(), "");
            LOG.debug("JMS message with ID: {} is sent to USM.", messageId);
            TextMessage response = consumer.getMessage(messageId, TextMessage.class);

            if (response != null) {
                GetUserContextResponse userContextResponse = JAXBMarshaller.unmarshallTextMessage(response, GetUserContextResponse.class);
                LOG.debug("Response concerning message with ID: {} is received.", messageId);
                userContext = userContextResponse.getContext();
            } else {
                LOG.error("Error occurred while receiving JMS response for message ID: {}", messageId);
                throw new IllegalArgumentException("Unable to receive a response from USM.");
            }
        } catch (ModelMarshallException | JAXBException e) {
            throw new IllegalArgumentException("Unexpected exception while trying to get user context.", e);
        } catch (MessageException e) {
            LOG.error("Unable to receive a response from USM.");
            throw new IllegalArgumentException("Unable to receive a response from USM.");
        }
        return userContext;
    }
    
    public String getOrganisationName(String username) throws ModelMarshallException, MessageException {
        GetContactDetailResponse userResponse = getContactDetails(username);
        if (userResponse != null && userResponse.getContactDetails() != null) {
            return userResponse.getContactDetails().getOrganisationName();
        } else {
            return null;
        }
    }
    
    public GetContactDetailResponse getContactDetails(String username) throws ModelMarshallException, MessageException {
        try {
            String userRequest = UserModuleRequestMapper.mapToGetContactDetailsRequest(username);
            String userMessageId = producer.sendModuleMessage(userRequest, responseQueue, UserModuleMethod.GET_CONTACT_DETAILS.value(), "");
            TextMessage userMessage = consumer.getMessage(userMessageId, TextMessage.class);
            return JAXBMarshaller.unmarshallTextMessage(userMessage, GetContactDetailResponse.class);
        } catch (JAXBException e) {
            throw new IllegalArgumentException(e);
        }
    }
    
    public FindOrganisationsResponse findOrganisation(String nationIsoName) throws MessageException {
        try {
            String userRequest = UserModuleRequestMapper.mapToFindOrganisationsRequest(nationIsoName);
            String userMessageId = producer.sendModuleMessage(userRequest, responseQueue, UserModuleMethod.FIND_ORGANISATIONS.value(), "");
            TextMessage userMessage = consumer.getMessage(userMessageId, TextMessage.class);
            return JAXBMarshaller.unmarshallTextMessage(userMessage, FindOrganisationsResponse.class);
        } catch (ModelMarshallException | JAXBException e) {
            throw new IllegalArgumentException(e);
        }
    }
    
    public List<RecipientInfoType> getRecipientInfoType(String nationIsoName, String dataflow) throws MessageException {
        FindOrganisationsResponse response = findOrganisation(nationIsoName);

        List<RecipientInfoType> recipientInfoList = new ArrayList<>();
        List<Organisation> organisations = response.getOrganisation();
        for (Organisation organisation : organisations) {
            List<EndPoint> endPoints = organisation.getEndPoints();
            for (EndPoint endPoint : endPoints) {
                for (Channel channel : endPoint.getChannels()) {
                    if (channel.getDataFlow().equals(dataflow)) {
                        RecipientInfoType recipientInfo = new RecipientInfoType();
                        recipientInfo.setKey(endPoint.getName());
                        recipientInfo.setValue(endPoint.getUri());
                        recipientInfoList.add(recipientInfo);
                    }
                }
            }
        }
        return recipientInfoList;
    }
}
