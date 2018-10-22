package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageException;
import eu.europa.ec.fisheries.uvms.commons.message.impl.AbstractProducer;
import eu.europa.ec.fisheries.uvms.config.exception.ConfigMessageException;
import eu.europa.ec.fisheries.uvms.config.message.ConfigMessageProducer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ejb.Stateless;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.jms.Destination;
import javax.transaction.Transactional;

@Stateless
public class ConfigMessageProducerBean extends AbstractProducer implements ConfigMessageProducer {

    private static final Logger LOG = LoggerFactory.getLogger(ConfigMessageProducerBean.class);


    public String getDestinationName() {
        return MessageConstants.QUEUE_CONFIG;
    }

    @Override
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public String sendConfigMessage(String text) throws ConfigMessageException {
        try {
            return sendMessageToSpecificQueueWithFunction(text, getDestination(), null, null , null);
        } catch (MessageException e) {
            LOG.error("[ Error when sending config message. ] {}", e.getMessage());
            throw new ConfigMessageException("[ Error when sending config message. ]");
        }
    }

    @Transactional(value = Transactional.TxType.REQUIRES_NEW)
    public String sendModuleMessage(String text, String function) throws MessageException {
        return this.sendMessageToSpecificQueueWithFunction(text, getDestination(), null, function, null);
    }

    @Transactional(value = Transactional.TxType.REQUIRES_NEW)
    public String sendModuleMessage(String text, Destination replyTo, String function, String grouping) throws MessageException {
        return this.sendMessageToSpecificQueueWithFunction(text, getDestination(), replyTo, function, grouping);
    }


}
