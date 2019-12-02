package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.commons.message.context.MappedDiagnosticContext;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.TicketDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.AssetNotSendingEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.AssetNotSendingUpdateEvent;
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

@Stateless
public class AssetNotSendingProducer {

    private static final Logger LOG = LoggerFactory.getLogger(AssetNotSendingProducer.class);

    @Resource(mappedName = "java:/" + MessageConstants.QUEUE_INCIDENT)
    private Destination destination;

    @Inject
    @JMSConnectionFactory("java:/ConnectionFactory")
    private JMSContext context;

    private ObjectMapper om = new ObjectMapper();

    @PostConstruct
    public void init() {
        om.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
        om.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        LOG.warn("DESTINATION: " + destination);
    }

    public void updatedTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @AssetNotSendingUpdateEvent TicketDto ticket) {
        sendEvent(ticket, "AssetNotSendingUpdate");
    }

    public void createdTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @AssetNotSendingEvent TicketDto ticket) {
        LOG.warn("ASSET NOT SENDING EVENT FIRED");
        LOG.warn("DESTINATION: " + destination);
        sendEvent(ticket, "AssetNotSending");
    }

    private void sendEvent(TicketDto ticketDto, String eventName) {
        try {
            String outgoingJson = om.writeValueAsString(ticketDto);
            TextMessage message = this.context.createTextMessage(outgoingJson);
            message.setStringProperty(MessageConstants.EVENT_STREAM_EVENT, eventName);
            MappedDiagnosticContext.addThreadMappedDiagnosticContextToMessageProperties(message);
            context.createProducer().setDeliveryMode(1).setTimeToLive(5000L).send(destination, message);
        } catch (Exception e) {
            LOG.error("Error while sending ticket event to event stream topic: ", e);
            throw new RuntimeException(e);
        }
    }

}