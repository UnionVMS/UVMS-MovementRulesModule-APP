package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.context.MappedDiagnosticContext;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.EventTicket;
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
import javax.jms.Destination;
import javax.jms.JMSConnectionFactory;
import javax.jms.JMSContext;
import javax.jms.TextMessage;
import java.util.ArrayList;
import java.util.List;

@Stateless
public class EventStreamSender {

    private static final Logger LOG = LoggerFactory.getLogger(EventStreamSender.class);

    @Resource(mappedName = "java:/" + MessageConstants.EVENT_STREAM_TOPIC)
    private Destination destination;

    @Inject
    @JMSConnectionFactory("java:/ConnectionFactory")
    JMSContext context;

    private ObjectMapper om = new ObjectMapper();

    @PostConstruct
    public void init() {
        om.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        om.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    }

    public void updatedTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketUpdateEvent EventTicket ticket) {
        sendEvent(ticket, "TicketUpdate");
    }
    
    public void createdTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketEvent EventTicket ticket) {
        sendEvent(ticket, "Ticket");
    }
    
    private void sendEvent(EventTicket eventTicket, String eventName) {
        if (eventTicket.getCustomRule() == null) {
            LOG.error("Rule in eventTicket {} is null", eventTicket.getTicket().getRuleName());
            return;
        }
        try {
            String outgoingJson = om.writeValueAsString(TicketMapper.toTicketType(eventTicket.getTicket()));
            List<String> subscriberList = new ArrayList<>();
            eventTicket.getCustomRule().getRuleSubscriptionList().stream()
                    .filter(sub -> sub.getType().equals(SubscriptionTypeType.TICKET.value()))
                    .forEach(sub -> subscriberList.add(sub.getOwner()));
            String subscriberJson = (eventTicket.getCustomRule().getAvailability().equals(AvailabilityType.GLOBAL.value()) ? null : om.writeValueAsString(subscriberList));


            TextMessage message = this.context.createTextMessage(outgoingJson);
            message.setStringProperty(MessageConstants.EVENT_STREAM_EVENT, eventName);
            message.setStringProperty(MessageConstants.EVENT_STREAM_SUBSCRIBER_LIST, subscriberJson);
            MappedDiagnosticContext.addThreadMappedDiagnosticContextToMessageProperties(message);

            context.createProducer().setDeliveryMode(1).setTimeToLive(5000L).send(destination, message);

        } catch (Exception e) {
            LOG.error("Error while sending ticket event to event stream topic: ", e);
            throw new RuntimeException(e);
        }
    }

}