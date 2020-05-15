package eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean;

import eu.europa.ec.fisheries.uvms.commons.date.JsonBConfigurator;
import eu.europa.ec.fisheries.uvms.commons.message.api.MessageConstants;
import eu.europa.ec.fisheries.uvms.incident.model.dto.IncidentTicketDto;
import eu.europa.ec.fisheries.uvms.incident.model.dto.TicketType;
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
import javax.json.bind.Jsonb;

@Stateless
public class IncidentProducer {

    private static final Logger LOG = LoggerFactory.getLogger(IncidentProducer.class);

    @Inject
    @JMSConnectionFactory("java:/ConnectionFactory")
    private JMSContext context;

    @Resource(mappedName = "java:/" + MessageConstants.QUEUE_INCIDENT)
    private Destination queue;

    private Jsonb jsonb;

    @PostConstruct
    public void init() {
        JsonBConfigurator configurator = new JsonBConfigurator();
        jsonb = configurator.getContext(null);
    }

    public void updatedTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketUpdateEvent EventTicket eventTicket) {
        IncidentTicketDto dto = mapToIncidentTicket(eventTicket, TicketType.ASSET_NOT_SENDING);
        send(dto, "IncidentUpdate");
    }

    public void createdTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketEvent EventTicket eventTicket) {
        IncidentTicketDto dto = mapToIncidentTicket(eventTicket, TicketType.ASSET_NOT_SENDING);
        send(dto, "Incident");
    }

    public void send(IncidentTicketDto ticket, String eventName) {
        try {
            String json = jsonb.toJson(ticket);
            TextMessage message = context.createTextMessage(json);
            message.setStringProperty("eventName", eventName);
            JMSProducer producer = context.createProducer();
            producer.setDeliveryMode(DeliveryMode.PERSISTENT).send(queue, message);
        } catch (Exception e) {
            LOG.error("Error while sending AssetNotSending event. {}", e.toString());
        }
    }

    private IncidentTicketDto mapToIncidentTicket(EventTicket eventTicket, TicketType ticketType){
        IncidentTicketDto dto = new IncidentTicketDto();
        Ticket ticket = eventTicket.getTicket();
        dto.setAssetId(ticket.getAssetGuid());
        dto.setChannelId(ticket.getChannelGuid());
        dto.setCreatedDate(ticket.getCreatedDate());
        dto.setId(ticket.getGuid());
        dto.setMobTermId(ticket.getMobileTerminalGuid());
        dto.setMovementId(ticket.getMovementGuid());
        //dto.setPollId(eventTicket.getPollId());
        dto.setRecipient(ticket.getRecipient());
        dto.setRuleGuid(ticket.getRuleGuid());
        dto.setRuleName(ticket.getRuleName());
        dto.setStatus(ticket.getStatus().value());
        dto.setTicketCount(ticket.getTicketCount());
        dto.setType(ticketType);
        dto.setUpdated(ticket.getUpdated());
        dto.setUpdatedBy(ticket.getUpdatedBy());

        return dto;
    }
}
