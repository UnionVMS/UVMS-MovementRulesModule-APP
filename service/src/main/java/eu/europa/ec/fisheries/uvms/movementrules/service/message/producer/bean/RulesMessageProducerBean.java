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
package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import javax.annotation.PostConstruct;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.jms.*;

import eu.europa.ec.fisheries.schema.movementrules.common.v1.RulesFault;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.commons.message.impl.AbstractProducer;
import eu.europa.ec.fisheries.uvms.commons.message.impl.JMSUtils;
import eu.europa.ec.fisheries.uvms.config.exception.ConfigMessageException;
import eu.europa.ec.fisheries.uvms.config.message.ConfigMessageProducer;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.constants.DataSourceQueue;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.RulesMessageProducer;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;

@Stateless
public class RulesMessageProducerBean extends AbstractProducer implements RulesMessageProducer, ConfigMessageProducer {

    private static final Logger LOG = LoggerFactory.getLogger(RulesMessageProducerBean.class);

    private Queue rulesResponseQueue;
    private Queue movementQueue;
    private Queue configQueue;
    private Queue assetQueue;
    private Queue mobileTerminalQueue;
    private Queue exchangeQueue;
    private Queue userQueue;
    private Queue auditQueue;

    @PostConstruct
    public void init() {
        rulesResponseQueue = JMSUtils.lookupQueue(MessageConstants.QUEUE_MOVEMENTRULES);
        movementQueue = JMSUtils.lookupQueue(MessageConstants.QUEUE_MODULE_MOVEMENT);
        configQueue = JMSUtils.lookupQueue(MessageConstants.QUEUE_CONFIG);
        assetQueue = JMSUtils.lookupQueue(MessageConstants.QUEUE_ASSET_EVENT);
        mobileTerminalQueue = JMSUtils.lookupQueue(MessageConstants.QUEUE_MOBILE_TERMINAL_EVENT);
        exchangeQueue = JMSUtils.lookupQueue(MessageConstants.QUEUE_EXCHANGE_EVENT);
        userQueue = JMSUtils.lookupQueue(MessageConstants.QUEUE_USM);
        auditQueue = JMSUtils.lookupQueue(MessageConstants.QUEUE_AUDIT_EVENT);
    }


    private static final String MOVEMENTRULES_QUEUE = "UVMSMovementRulesEvent";
    private static final String RESPONSE_QUEUE = "IntegrationTestsResponseQueue";
    public String sendResponseMessageForTest(String text, String requestType) {
        try {
            Connection connection = this.getConnectionFactory().createConnection();

            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
            Queue responseQueue = session.createQueue(RESPONSE_QUEUE);
            Queue assetQueue = session.createQueue(RESPONSE_QUEUE);

            TextMessage message = session.createTextMessage();
            message.setJMSReplyTo(responseQueue);
            message.setText(text);
            message.setStringProperty("FUNCTION", requestType);

            session.createProducer(assetQueue).send(message);
            connection.close();

            return message.getJMSMessageID();
        }catch(Exception e){
            throw new RuntimeException(e);
        } finally {

        }
    }

    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public String  sendDataSourceMessage(String text, DataSourceQueue queue) throws MessageException  {
        return sendDataSourceMessage(text, queue, null, null);
    }
    
    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public String sendDataSourceMessage(String text, DataSourceQueue queue, String function, String grouping) throws MessageException  {
        LOG.debug("Sending message to {}", queue.name());
        try {
            Queue destination = getDestinationQueue(queue);
            if(destination != null){
                return sendMessageToSpecificQueueWithFunction(text, destination, rulesResponseQueue, function, grouping);

            }
            LOG.error("Silent throw");
            //return null;
            throw new RuntimeException("Got no Queue");
        } catch (Exception e) {
            LOG.error("[ Error when sending message. ] {}", e);
            throw new MessageException("[ Error when sending message. ]", e);
        }
    }

    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public String sendConfigMessage(String text) throws ConfigMessageException {
        try {
            return sendDataSourceMessage(text, DataSourceQueue.CONFIG, "", "");
        } catch (MessageException  e) {
            LOG.error("[ Error when sending config message. ] {}", e.getMessage());
            throw new ConfigMessageException("[ Error when sending config message. ]");
        }
    }


    @Override
    public void sendModuleErrorResponseMessage(RulesFault fault, TextMessage message) {
        try {
            LOG.debug("Sending error message back from Rules module to recipient on JMS Queue with correlationID: {} ", message.getJMSMessageID());
            String data = JAXBMarshaller.marshallJaxBObjectToString(fault);
            this.sendResponseMessageToSender(message, data, "Rules");
        } catch (MovementRulesModelMarshallException | JMSException | MessageException e) {
            LOG.error("Error when returning Error message to recipient");
        }
    }

    private Queue getDestinationQueue(DataSourceQueue queue) {
        Queue destination = null;
        switch (queue) {
            case MOVEMENT:
                destination = movementQueue;
                break;
            case CONFIG:
                destination = configQueue;
                break;
            case ASSET:
                destination = assetQueue;
                break;
            case MOBILE_TERMINAL:
                destination = mobileTerminalQueue;
                break;
            case EXCHANGE:
                destination = exchangeQueue;
                break;
            case USER:
                destination = userQueue;
                break;
            case AUDIT:
                destination = auditQueue;
                break;
            default:
                break;
        }
        return destination;
    }

    @Override
    public String getDestinationName() {
        return MessageConstants.QUEUE_MOVEMENTRULES;
    }

}

