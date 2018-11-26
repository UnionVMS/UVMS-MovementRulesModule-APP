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
package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.Queue;
import javax.jms.Session;

@Stateless
public class JMSHelper {

    private static final long TIMEOUT = 20000;

    @Resource(name = "java:/ConnectionFactory")
    private ConnectionFactory connectionFactory;
    
    @Resource(name = "java:/jms/queue/UVMSExchangeEvent")
    private Queue exchangeQueue;
    
    public Message getMessageFromExchangeQueue() throws Exception {
        Connection connection = connectionFactory.createConnection();
        try {
            connection.start();
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            return session.createConsumer(exchangeQueue).receive(TIMEOUT);
        } finally {
            connection.close();
        }
    }

    public void clearExchangeQueue() throws Exception {
        Connection connection = connectionFactory.createConnection();
        MessageConsumer consumer;
        try {
            connection.start();
            Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
            consumer = session.createConsumer(exchangeQueue);

            while (consumer.receive(1000L) != null);
        } finally {
            connection.close();
        }
    }
}
