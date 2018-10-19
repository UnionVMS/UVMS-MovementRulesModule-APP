package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.commons.message.impl.AbstractProducer;

import javax.ejb.Stateless;
import javax.jms.Destination;
import javax.transaction.Transactional;

@Stateless
public class ExchangeProducerBean extends AbstractProducer {

    @Override
    public String getDestinationName() {
        return MessageConstants.QUEUE_EXCHANGE_EVENT;
    }

    @Transactional(value = Transactional.TxType.REQUIRES_NEW)
    public String sendModuleMessage(String text, String function) throws MessageException {
        return this.sendMessageToSpecificQueueWithFunction(text, getDestination(), null, function, null);
    }
    @Transactional(value = Transactional.TxType.REQUIRES_NEW)
    public String sendModuleMessage(String text, Destination replyTo, String function) throws MessageException {
        return this.sendMessageToSpecificQueueWithFunction(text, getDestination(), replyTo, function, null);
    }
}
