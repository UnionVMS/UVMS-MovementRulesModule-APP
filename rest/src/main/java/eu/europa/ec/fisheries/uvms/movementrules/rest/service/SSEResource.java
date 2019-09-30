package eu.europa.ec.fisheries.uvms.movementrules.rest.service;

import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.ejb.TransactionAttribute;
import javax.ejb.TransactionAttributeType;
import javax.enterprise.event.Observes;
import javax.enterprise.event.TransactionPhase;
import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.sse.OutboundSseEvent;
import javax.ws.rs.sse.Sse;
import javax.ws.rs.sse.SseEventSink;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketUpdateEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.TicketMapper;
import eu.europa.ec.fisheries.uvms.rest.security.RequiresFeature;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;

@Singleton
@Startup
@Path("sse")
@RequiresFeature(UnionVMSFeature.viewAlarmsOpenTickets)
public class SSEResource {

    private static final Logger LOG = LoggerFactory.getLogger(SSEResource.class);

    private ConcurrentLinkedQueue<UserSseEventSink> userSinks = new ConcurrentLinkedQueue<>();

    @Context
    private Sse sse;
    
    @Inject
    private RulesServiceBean rulesService;
    
    @GET
    @Path("subscribe")
    @Produces(MediaType.SERVER_SENT_EVENTS)
    public void subscribe(@Context SseEventSink sseEventSink, @Context SecurityContext securityContext) {
        sseEventSink.send(sse.newEvent("UVMS SSE Ticket notifications"));
        String user = securityContext.getUserPrincipal().getName();
        userSinks.add(new UserSseEventSink(user, sseEventSink));
        sseEventSink.send(sse.newEvent("User " + user + " is now registered"));
    }

    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public void updatedTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketUpdateEvent Ticket ticket) {
        sendEvent(ticket, "TicketUpdate");
    }
    
    @TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)
    public void createdTicket(@Observes(during = TransactionPhase.AFTER_SUCCESS) @TicketEvent Ticket ticket) {
        sendEvent(ticket, "Ticket");
    }
    
    private void sendEvent(Ticket ticket, String eventName) {
        CustomRule customRule = rulesService.getCustomRuleByGuid(UUID.fromString(ticket.getRuleGuid()));
        if (customRule == null) {
            LOG.error("Could not find rule with id: {}", ticket.getRuleGuid());
            return;
        }
        OutboundSseEvent sseEvent = createSseEvent(ticket, eventName);
        
        userSinks.stream().forEach(userSink -> {
            if (userSink.getEventSink().isClosed()) {
                userSinks.remove(userSink);
            }
        });
        
        userSinks.stream().forEach(userSink -> {
            for (RuleSubscription subscription : customRule.getRuleSubscriptionList()) {
                if ((userSink.getUser().equals(subscription.getOwner()) && subscription.getType().equals(SubscriptionTypeType.TICKET.value())) 
                        || customRule.getAvailability().equals(AvailabilityType.GLOBAL.value())) {
                    LOG.debug("Broadcasting to {}", subscription.getOwner());
                    userSink.getEventSink().send(sseEvent).whenComplete((object, error) -> {
                        if (error != null) {
                            userSinks.remove(userSink);
                        }
                     });
                }
            }
        });
        LOG.debug("userSinks size: {}", userSinks.size());
    }

    private OutboundSseEvent createSseEvent(Ticket ticket, String eventName) {
        return sse.newEventBuilder()
                .name(eventName)
                .id(String.valueOf(System.currentTimeMillis()))
                .mediaType(MediaType.APPLICATION_JSON_PATCH_JSON_TYPE)
                .data(Ticket.class, TicketMapper.toTicketType(ticket))
                .build();
    }

    private class UserSseEventSink {
        private String user;
        private SseEventSink eventSink;
        
        public UserSseEventSink(String user, SseEventSink sseEventSink) {
            this.user = user;
            this.eventSink = sseEventSink;
        }

        public String getUser() {
            return user;
        }
        
        public SseEventSink getEventSink() {
            return eventSink;
        }
    }
}