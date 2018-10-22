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

import javax.ejb.Stateless;
import javax.jms.*;

import eu.europa.ec.fisheries.schema.movementrules.common.v1.RulesFault;
import eu.europa.ec.fisheries.uvms.commons.message.impl.AbstractProducer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;

@Stateless
public class RulesMessageProducerBean extends AbstractProducer {

    private static final Logger LOG = LoggerFactory.getLogger(RulesMessageProducerBean.class);

    public void sendModuleErrorResponseMessage(RulesFault fault, TextMessage message) {
        try {
            LOG.debug("Sending error message back from Rules module to recipient on JMS Queue with correlationID: {} ", message.getJMSMessageID());
            String data = JAXBMarshaller.marshallJaxBObjectToString(fault);
            this.sendResponseMessageToSender(message, data, "Rules");
        } catch (MovementRulesModelMarshallException | JMSException | MessageException e) {
            LOG.error("Error when returning Error message to recipient");
        }
    }

    public String getDestinationName() {
        return MessageConstants.QUEUE_MOVEMENTRULES;
    }

}

