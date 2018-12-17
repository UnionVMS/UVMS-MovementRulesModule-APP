package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.commons.message.impl.AbstractProducer;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.jms.Destination;

@Stateless
public class AuditProducerBean extends AbstractProducer {

    @Override
    public String getDestinationName() {
        return MessageConstants.QUEUE_AUDIT_EVENT;
    }


    public String sendModuleMessage(String text, Destination replyTo, String function, String grouping) throws MessageException {
        return this.sendMessageToSpecificQueueWithFunction(text, getDestination(), replyTo, function, grouping);
    }

}
