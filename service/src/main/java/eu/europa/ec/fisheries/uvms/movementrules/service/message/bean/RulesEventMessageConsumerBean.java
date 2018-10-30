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
package eu.europa.ec.fisheries.uvms.movementrules.service.message.bean;

import java.util.UUID;
import javax.ejb.ActivationConfigProperty;
import javax.ejb.MessageDriven;
import javax.inject.Inject;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;

import eu.europa.ec.fisheries.schema.movementrules.module.v1.*;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesEventServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.RulesMessageProducerBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.context.MappedDiagnosticContext;
import eu.europa.ec.fisheries.uvms.movementrules.model.constant.FaultCode;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.MovementRulesModuleResponseMapper;

@MessageDriven(mappedName = MessageConstants.QUEUE_MOVEMENTRULES_EVENT, activationConfig = {
        @ActivationConfigProperty(propertyName = MessageConstants.MESSAGING_TYPE_STR, propertyValue = MessageConstants.CONNECTION_TYPE),
        @ActivationConfigProperty(propertyName = MessageConstants.DESTINATION_TYPE_STR, propertyValue = MessageConstants.DESTINATION_TYPE_QUEUE),
        @ActivationConfigProperty(propertyName = MessageConstants.DESTINATION_STR, propertyValue = MessageConstants.QUEUE_MOVEMENTRULES_EVENT_NAME)
})
public class RulesEventMessageConsumerBean implements MessageListener {

    private static final Logger LOG = LoggerFactory.getLogger(RulesEventMessageConsumerBean.class);


    @Inject
    private RulesEventServiceBean rulesEventServiceBean;


    @Inject
    private RulesMessageProducerBean rulesProducer;


    @Override
    public void onMessage(Message message) {
        String id = UUID.randomUUID().toString();
        MDC.put("clientName", id);
        MDC.remove("requestId");
        LOG.debug("Message received in rules. Times redelivered: {}", getTimesRedelivered(message));
        TextMessage textMessage = (TextMessage) message;
        MappedDiagnosticContext.addMessagePropertiesToThreadMappedDiagnosticContext(textMessage);
        try {
            RulesModuleMethod method = RulesModuleMethod.fromValue(textMessage.getStringProperty(MessageConstants.JMS_FUNCTION_PROPERTY));
            if (method == null) {
                throw new NullPointerException("[ Request method is null ]");
            }

            LOG.info("Request message method: {}", method.value());
            switch (method) {
                case SET_MOVEMENT_REPORT:
                    SetMovementReportRequest request = JAXBMarshaller.unmarshallTextMessage(textMessage, SetMovementReportRequest.class);
                    rulesEventServiceBean.setMovementReportReceived(request);
                    break;
                case PING:
                    PingResponse pingResponse = rulesEventServiceBean.pingReceived();
                    String pingResponseText = JAXBMarshaller.marshallJaxBObjectToString(pingResponse);
                    rulesProducer.sendModuleResponseMessage(textMessage, pingResponseText);
                    break;
                case EVALUATE_RULES:
                    rulesEventServiceBean.evaluateCustomRules(textMessage);
                default:
                    LOG.error("[ Request method '{}' is not implemented ]", method.name());
                     rulesProducer.sendModuleErrorResponseMessage(MovementRulesModuleResponseMapper.createFaultMessage(FaultCode.RULES_MESSAGE, "Method not implemented:" + method.name()), textMessage);
                    break;
            }

        } catch (Exception e) {
            LOG.error("[ Error when receiving message in rules: {}]", e);
            rulesProducer.sendModuleErrorResponseMessage(MovementRulesModuleResponseMapper.createFaultMessage(FaultCode.RULES_MESSAGE, "Error when receiving message in rules:" + e), textMessage);
        } finally {
            MDC.remove("clientName");
        }
    }

    private int getTimesRedelivered(Message message) {
        try {
            return (message.getIntProperty("JMSXDeliveryCount") - 1);
        } catch (Exception e) {
            return 0;
        }
    }

}
