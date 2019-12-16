package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.service.exception.ObjectMapperContextResolver;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.EventTicket;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketUpdateEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.TicketMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.enterprise.event.Observes;
import javax.enterprise.event.TransactionPhase;
import javax.inject.Inject;
import javax.jms.*;

@Stateless
public class IncidentProducer {

    private static final Logger LOG = LoggerFactory.getLogger(IncidentProducer.class);

    @Inject
    @JMSConnectionFactory("java:/ConnectionFactory")
    private JMSContext context;

    @Resource(mappedName = "java:/" + MessageConstants.QUEUE_INCIDENT)
    private Destination queue;

    private ObjectMapper om;

    @PostConstruct
    public void init() {
        ObjectMapperContextResolver resolver = new ObjectMapperContextResolver();
        om = resolver.getContext(null);
    }

    public void updatedTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketUpdateEvent EventTicket eventTicket) {
        send(eventTicket.getTicket(), "IncidentUpdate");
    }

    public void createdTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketEvent EventTicket eventTicket) {
        send(eventTicket.getTicket(), "Incident");
    }

    public void send(Ticket ticket, String eventName) {
        try {
            String json = om.writeValueAsString(TicketMapper.toTicketType(ticket));
            TextMessage message = context.createTextMessage(json);
            message.setStringProperty("eventName", eventName);
            JMSProducer producer = context.createProducer();
            producer.setDeliveryMode(DeliveryMode.PERSISTENT).send(queue, message);
        } catch (JMSException | JsonProcessingException e) {
            LOG.error("Error while sending AssetNotSending event. {}", e.toString());
        }
    }
}
