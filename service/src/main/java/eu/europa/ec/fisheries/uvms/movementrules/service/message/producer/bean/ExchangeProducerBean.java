package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.impl.AbstractProducer;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Queue;

@Stateless
public class ExchangeProducerBean extends AbstractProducer {

    @Resource(mappedName =  "java:/" + MessageConstants.QUEUE_EXCHANGE_EVENT)
    private Destination destination;

    @Override
    public Destination getDestination() {
        return destination;
    }

    @Resource(mappedName = "java:/" + MessageConstants.QUEUE_MOVEMENTRULES)
    private Queue responseQueue;

    public String sendModuleMessage(String text, String function) throws JMSException {
        return this.sendMessageToSpecificQueueWithFunction(text, getDestination(), null, function, null);
    }

    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public String sendSynchronousModuleMessage(String text, String function) throws JMSException {
        return this.sendMessageToSpecificQueueWithFunction(text, getDestination(), responseQueue, function, "");
    }

}
