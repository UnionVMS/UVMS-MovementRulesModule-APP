package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.commons.message.impl.AbstractProducer;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.jms.Destination;
import javax.jms.Queue;

@Stateless
public class ExchangeProducerBean extends AbstractProducer {

    @Resource(name = "java:/jms/queue/UVMSExchangeEvent")
    private Queue exchangeQueue;
    
    @Override
    public String getDestinationName() {
        return MessageConstants.QUEUE_EXCHANGE_EVENT;
    }

    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public String sendModuleMessage(String text, String function) throws MessageException {
        return this.sendMessageToSpecificQueueWithFunction(text, exchangeQueue, null, function, null);
    }

    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public String sendModuleMessage(String text, Destination replyTo, String function) throws MessageException {
        return this.sendMessageToSpecificQueueWithFunction(text, exchangeQueue, replyTo, function, "");
    }

}
