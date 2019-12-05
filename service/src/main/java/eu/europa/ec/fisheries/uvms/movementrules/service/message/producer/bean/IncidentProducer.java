package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.EventTicket;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketUpdateEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.TicketMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.enterprise.context.Dependent;
import javax.enterprise.event.Observes;
import javax.enterprise.event.TransactionPhase;
import javax.jms.*;

@Dependent
public class IncidentProducer {

    private static final Logger LOG = LoggerFactory.getLogger(IncidentProducer.class);

    @Resource(mappedName = "java:/ConnectionFactory")
    private ConnectionFactory connectionFactory;

    @Resource(mappedName = "java:/" + MessageConstants.QUEUE_INCIDENT)
    private Queue queue;

    private ObjectMapper om = new ObjectMapper();

    @PostConstruct
    public void init() {
        om.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        om.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    }

    public void updatedTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketUpdateEvent EventTicket ticket) throws JsonProcessingException {
        String json = om.writeValueAsString(TicketMapper.toTicketType(ticket.getTicket()));
        send(json, "IncidentUpdate");
    }

    public void createdTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketEvent EventTicket ticket) throws JsonProcessingException {
        String json = om.writeValueAsString(TicketMapper.toTicketType(ticket.getTicket()));
        send(json, "Incident");
    }

    public void send(String json, String eventName) {
        try (JMSContext context = connectionFactory.createContext()) {
            TextMessage message = context.createTextMessage(json);
            message.setStringProperty("eventName", eventName);
            JMSProducer producer = context.createProducer();
            producer.setDeliveryMode(DeliveryMode.PERSISTENT).setTimeToLive(5000L).send(queue, message);
        } catch (JMSException e) {
            LOG.error("Error while sending AssetNotSending event. {}", e.toString());
        }
    }
}
