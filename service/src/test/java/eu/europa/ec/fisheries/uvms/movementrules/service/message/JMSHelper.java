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
package eu.europa.ec.fisheries.uvms.movementrules.service.message;

import javax.jms.*;

import org.apache.activemq.ActiveMQConnectionFactory;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;

public class JMSHelper {

    private static final long TIMEOUT = 20000;

    private ConnectionFactory connectionFactory = new ActiveMQConnectionFactory("tcp://localhost:61616");

    public String sendMessageToRules(String text, String requestType, String resQueue) throws Exception {
        Connection connection = connectionFactory.createConnection();
        try {
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
            Queue responseQueue = session.createQueue(resQueue);
            Queue movementRulesEventQueue = session.createQueue(MessageConstants.QUEUE_MOVEMENTRULES_EVENT_NAME);

            TextMessage message = session.createTextMessage();
            message.setJMSReplyTo(responseQueue);
            message.setText(text);
            message.setStringProperty(MessageConstants.JMS_FUNCTION_PROPERTY, requestType);

            session.createProducer(movementRulesEventQueue).send(message);

            return message.getJMSMessageID();
        } finally {
            connection.close();
        }
    }

    public Message listenForResponseOnQueue(String correlationId, String queue) throws Exception {
        Connection connection = connectionFactory.createConnection();
        try {
            connection.start();
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
            Queue responseQueue = session.createQueue(queue);

            return session.createConsumer(responseQueue).receive(TIMEOUT);
        } finally {
            connection.close();
        }
    }

    public void clearQueue(String queue) throws Exception {
        Connection connection = connectionFactory.createConnection();
        MessageConsumer consumer;
        try {
            do {
                connection.start();
                Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
                Queue responseQueue = session.createQueue(queue);
                consumer = session.createConsumer(responseQueue);
            }
            while (consumer.receive(1L) != null);
        } finally {
            connection.close();
        }
    }
}
