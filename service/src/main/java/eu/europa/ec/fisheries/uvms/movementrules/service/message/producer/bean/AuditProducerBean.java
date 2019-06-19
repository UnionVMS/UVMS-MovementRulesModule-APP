package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.impl.AbstractProducer;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Queue;

@Stateless
public class AuditProducerBean extends AbstractProducer {

    @Resource(mappedName =  "java:/" + MessageConstants.QUEUE_AUDIT_EVENT)
    private Queue destination;

    @Override
    public Destination getDestination() {
        return destination;
    }

    public String sendModuleMessage(String text, Destination replyTo, String function, String grouping) throws JMSException {
        return this.sendMessageToSpecificQueueWithFunction(text, getDestination(), replyTo, function, grouping);
    }
}