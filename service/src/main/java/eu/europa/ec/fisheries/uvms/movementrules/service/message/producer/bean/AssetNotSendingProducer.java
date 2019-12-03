package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.TicketDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.AssetNotSendingEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.AssetNotSendingUpdateEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.enterprise.context.Dependent;
import javax.enterprise.event.Observes;
import javax.jms.*;

@Dependent
public class AssetNotSendingProducer {

    private static final Logger LOG = LoggerFactory.getLogger(AssetNotSendingProducer.class);

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

    public void updatedTicket(@Observes @AssetNotSendingUpdateEvent TicketDto ticket) throws JsonProcessingException {
        String json = om.writeValueAsString(ticket);
        send(json, "AssetNotSendingUpdate");
    }

    public void createdTicket(@Observes @AssetNotSendingEvent TicketDto ticket) throws JsonProcessingException {
        String json = om.writeValueAsString(ticket);
        send(json, "AssetNotSending");
    }

    public void send(String json, String eventType) {
        try (JMSContext context = connectionFactory.createContext()) {
            TextMessage message = context.createTextMessage(json);
            message.setStringProperty("eventType", eventType);
            JMSProducer producer = context.createProducer();
            producer.send(queue, message);
        } catch (JMSException e) {
            LOG.error("Error while sending AssetNotSending event. {}", e.toString());
        }
    }
}
