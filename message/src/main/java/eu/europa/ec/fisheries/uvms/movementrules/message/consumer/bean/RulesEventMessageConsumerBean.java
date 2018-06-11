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
package eu.europa.ec.fisheries.uvms.movementrules.message.consumer.bean;

import java.util.UUID;
import javax.ejb.ActivationConfigProperty;
import javax.ejb.MessageDriven;
import javax.enterprise.event.Event;
import javax.inject.Inject;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.RulesBaseRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.RulesModuleMethod;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.context.MappedDiagnosticContext;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.ErrorEvent;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.GetTicketsAndRulesByMovementsEvent;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.PingReceivedEvent;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.SetMovementReportReceivedEvent;
import eu.europa.ec.fisheries.uvms.movementrules.message.event.carrier.EventMessage;
import eu.europa.ec.fisheries.uvms.movementrules.model.constant.FaultCode;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.RulesModelMarshallException;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.JAXBMarshaller;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.ModuleResponseMapper;

@MessageDriven(mappedName = MessageConstants.QUEUE_MODULE_RULES, activationConfig = {
        @ActivationConfigProperty(propertyName = MessageConstants.MESSAGING_TYPE_STR, propertyValue = MessageConstants.CONNECTION_TYPE),
        @ActivationConfigProperty(propertyName = MessageConstants.DESTINATION_TYPE_STR, propertyValue = MessageConstants.DESTINATION_TYPE_QUEUE),
        @ActivationConfigProperty(propertyName = MessageConstants.DESTINATION_STR, propertyValue = MessageConstants.RULES_MESSAGE_IN_QUEUE_NAME)
})
public class RulesEventMessageConsumerBean implements MessageListener {

    private static final Logger LOG = LoggerFactory.getLogger(RulesEventMessageConsumerBean.class);

    @Inject
    @SetMovementReportReceivedEvent
    private Event<EventMessage> setMovementReportRecievedEvent;

    @Inject
    @GetTicketsAndRulesByMovementsEvent
    private Event<EventMessage> getTicketsAndRulesByMovementsEvent;

    @Inject
    @PingReceivedEvent
    private Event<EventMessage> pingReceivedEvent;

    @Inject
    @ErrorEvent
    private Event<EventMessage> errorEvent;

    @Override
    public void onMessage(Message message) {
        String id = UUID.randomUUID().toString();
        MDC.put("clientName", id);
        MDC.remove("requestId");
        LOG.debug("Message received in rules. Times redelivered: {}", getTimesRedelivered(message));
        TextMessage textMessage = (TextMessage) message;
        MappedDiagnosticContext.addMessagePropertiesToThreadMappedDiagnosticContext(textMessage);
        try {
            RulesBaseRequest request = JAXBMarshaller.unmarshallTextMessage(textMessage, RulesBaseRequest.class);
            RulesModuleMethod method = request.getMethod();
            if (method == null) {
                throw new NullPointerException("[ Request method is null ]");
            }

            LOG.info("Request message method: {}", method.value());
            switch (method) {
                case SET_MOVEMENT_REPORT:
                    setMovementReportRecievedEvent.fire(new EventMessage(textMessage));
                    break;
                case GET_TICKETS_AND_RULES_BY_MOVEMENTS:
                    getTicketsAndRulesByMovementsEvent.fire(new EventMessage(textMessage));
                    break;
                case PING:
                    pingReceivedEvent.fire(new EventMessage(textMessage));
                    break;
                 default:
                    LOG.error("[ Request method '{}' is not implemented ]", method.name());
                    errorEvent.fire(new EventMessage(textMessage, ModuleResponseMapper.createFaultMessage(FaultCode.RULES_MESSAGE, "Method not implemented:" + method.name())));
                    break;
            }

        } catch (NullPointerException | RulesModelMarshallException e) {
            LOG.error("[ Error when receiving message in rules: {}]", e.getMessage());
            errorEvent.fire(new EventMessage(textMessage, ModuleResponseMapper.createFaultMessage(FaultCode.RULES_MESSAGE, "Error when receiving message in rules:" + e.getMessage())));
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
