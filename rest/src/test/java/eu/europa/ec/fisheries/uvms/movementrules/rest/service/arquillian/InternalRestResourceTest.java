package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.CustomRuleMapper;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

@RunWith(Arquillian.class)
public class InternalRestResourceTest extends BuildRulesRestDeployment {

    @Inject
    private RulesDao rulesDao;

    @Test
    public void getTicketsAndRulesByMovementsEventTest() throws Exception {

        final List<String> movementGuidList = new ArrayList<>();
        final String movementGuid = UUID.randomUUID().toString();
        movementGuidList.add(movementGuid);

        // Create a CustomRule
        CustomRule customRule = CustomRuleMapper.toCustomRuleEntity(RulesTestHelper.getCompleteNewCustomRule());
        customRule.setGuid(UUID.randomUUID().toString());
        customRule.setAvailability(AvailabilityType.GLOBAL);
        customRule.setUpdatedBy("TestUser");
        RuleSubscription ruleSubscription = new RuleSubscription();
        ruleSubscription.setType(SubscriptionTypeType.TICKET);
        ruleSubscription.setCustomRule(customRule);
        ruleSubscription.setOwner("TestUser");
        customRule.getRuleSubscriptionList().add(ruleSubscription);
        customRule = rulesDao.createCustomRule(customRule);

        assertNotNull(customRule);

        // Create a Ticket
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleGuid(customRule.getGuid());
        ticket.setUpdatedBy("TestUser");
        ticket.setMovementGuid(movementGuid);
        ticket = rulesDao.createTicket(ticket);

        assertNotNull(ticket);

        GetTicketsAndRulesByMovementsRequest request = new GetTicketsAndRulesByMovementsRequest();
        request.getMovementGuids().addAll(movementGuidList);

        String response = getWebTarget()
                .path("internal")
                .path("tickets-and-rules-by-movement")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(request), String.class);

        assertNotNull(response);

        GetTicketsAndRulesByMovementsResponse retVal =
                RulesTestHelper.deserializeResponseDto(response, GetTicketsAndRulesByMovementsResponse.class);
        String retrievedMovementGuid = retVal.getTicketsAndRules().get(0).getTicket().getMovementGuid();
        assertEquals(movementGuid, retrievedMovementGuid);

        rulesDao.removeTicketAfterTests(ticket);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }
}
