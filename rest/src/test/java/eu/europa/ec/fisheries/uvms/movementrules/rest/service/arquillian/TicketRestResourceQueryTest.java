package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketSearchKey;
import eu.europa.ec.fisheries.uvms.movementrules.rest.filter.AppError;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.BuildRulesRestDeployment;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.CustomRuleMapper;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class TicketRestResourceQueryTest extends BuildRulesRestDeployment {

    @Inject
    private RulesDao rulesDao;

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListByTicketQuery_ShouldFailInvalidTestQueryTest() {
        Response response = getWebTarget()
                .path("tickets/list/" + "testUser")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(new TicketQuery()));

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
        AppError appError = response.readEntity(AppError.class);
        assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), appError.code.intValue());
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListByTicketQueryTest() {
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.RULE_NAME, "Nonvalid rule name");
        query.getTicketSearchCriteria().add(criteria);

        GetTicketListByQueryResponse ticketList = sendTicketQuery(query);

        assertNotNull(ticketList.getTickets());
        int numberOfTickets = ticketList.getTickets().size();

        CustomRule customRule = createCustomRule("testUser");
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleName(customRule.getName());
        ticket.setRuleGuid(customRule.getGuid().toString());
        ticket.setUpdatedBy("testUser");

        Ticket createdTicket = rulesDao.createTicket(ticket);
        criteria.setValue(customRule.getName());
        ticketList = sendTicketQuery(query);

        assertThat(ticketList.getTickets().size(), is(numberOfTickets + 1));

        rulesDao.removeTicketAfterTests(createdTicket);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListByRuleGuidTest() {

        CustomRule customRule = createCustomRule("testUser");
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleName(customRule.getName());
        ticket.setRuleGuid(customRule.getGuid().toString());
        ticket.setUpdatedBy("testUser");

        Ticket createdTicket = rulesDao.createTicket(ticket);

        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.RULE_GUID, customRule.getGuid().toString());
        query.getTicketSearchCriteria().add(criteria);

        GetTicketListByQueryResponse ticketList = sendTicketQuery(query);

        assertTrue(ticketList.getTickets().stream().anyMatch(tic -> tic.getGuid().equals(createdTicket.getGuid().toString())));

        rulesDao.removeTicketAfterTests(createdTicket);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListByAssetGuidAndStatusTest() {

        CustomRule customRule = createCustomRule("testUser");
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleName(customRule.getName());
        ticket.setRuleGuid(customRule.getGuid().toString());
        ticket.setUpdatedBy("testUser");

        Ticket createdTicket = rulesDao.createTicket(ticket);

        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.ASSET_GUID, createdTicket.getAssetGuid());
        query.getTicketSearchCriteria().add(criteria);
        criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.STATUS, createdTicket.getStatus().toString());
        query.getTicketSearchCriteria().add(criteria);

        GetTicketListByQueryResponse ticketList = sendTicketQuery(query);

        assertTrue(ticketList.getTickets().stream().anyMatch(tic -> tic.getGuid().equals(createdTicket.getGuid().toString())));

        rulesDao.removeTicketAfterTests(createdTicket);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListByAssetGuidAndStatusWithTicketFromAnotherUserTest() {

        CustomRule customRule1 = createCustomRule("testUser");
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleName(customRule1.getName());
        ticket.setRuleGuid(customRule1.getGuid().toString());
        ticket.setUpdatedBy("testUser");

        Ticket createdTicket1 = rulesDao.createTicket(ticket);

        CustomRule customRule2 = createCustomRule("another testUser");
        ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleName(customRule2.getName());
        ticket.setRuleGuid(customRule2.getGuid().toString());
        ticket.setAssetGuid(createdTicket1.getAssetGuid());
        ticket.setUpdatedBy("another testUser");

        Ticket createdTicket2 = rulesDao.createTicket(ticket);

        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.ASSET_GUID, createdTicket1.getAssetGuid());
        query.getTicketSearchCriteria().add(criteria);
        criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.STATUS, createdTicket1.getStatus().toString());
        query.getTicketSearchCriteria().add(criteria);

        GetTicketListByQueryResponse ticketList = sendTicketQuery(query);

        assertTrue(ticketList.getTickets().stream().anyMatch(tic -> tic.getGuid().equals(createdTicket1.getGuid().toString())));
        assertFalse(ticketList.getTickets().stream().anyMatch(tic -> tic.getGuid().equals(createdTicket2.getGuid().toString())));

        rulesDao.removeTicketAfterTests(createdTicket1);
        rulesDao.removeCustomRuleAfterTests(customRule1);
        rulesDao.removeTicketAfterTests(createdTicket2);
        rulesDao.removeCustomRuleAfterTests(customRule2);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListByRuleRecipientAndFromDateTest() {

        CustomRule customRule1 = createCustomRule("testUser");
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleName(customRule1.getName());
        ticket.setRuleGuid(customRule1.getGuid().toString());
        ticket.setUpdatedBy("testUser");

        Ticket createdTicket1 = rulesDao.createTicket(ticket);

        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.RULE_RECIPIENT, createdTicket1.getRecipient());
        query.getTicketSearchCriteria().add(criteria);
        criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.FROM_DATE, "" + createdTicket1.getUpdated().toEpochMilli());
        query.getTicketSearchCriteria().add(criteria);

        GetTicketListByQueryResponse ticketList = sendTicketQuery(query);

        assertTrue(ticketList.getTickets().stream().anyMatch(tic -> tic.getGuid().equals(createdTicket1.getGuid().toString())));

        rulesDao.removeTicketAfterTests(createdTicket1);
        rulesDao.removeCustomRuleAfterTests(customRule1);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListByStatusAndToDateGetSeveralTickets() {

        CustomRule customRule1 = createCustomRule("testUser");
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleName(customRule1.getName());
        ticket.setRuleGuid(customRule1.getGuid().toString());
        ticket.setUpdatedBy("testUser");

        Ticket createdTicket1 = rulesDao.createTicket(ticket);

        CustomRule customRule2 = createCustomRule("testUser");
        ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleName(customRule2.getName());
        ticket.setRuleGuid(customRule2.getGuid().toString());
        ticket.setUpdatedBy("testUser");

        Ticket createdTicket2 = rulesDao.createTicket(ticket);

        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.STATUS, createdTicket1.getStatus().toString());
        query.getTicketSearchCriteria().add(criteria);
        criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.TO_DATE, "" + createdTicket2.getUpdated().plusSeconds(1).toEpochMilli());
        query.getTicketSearchCriteria().add(criteria);

        GetTicketListByQueryResponse ticketList = sendTicketQuery(query);

        assertTrue(ticketList.getTickets().stream().anyMatch(tic -> tic.getGuid().equals(createdTicket1.getGuid().toString())));
        assertTrue(ticketList.getTickets().stream().anyMatch(tic -> tic.getGuid().equals(createdTicket2.getGuid().toString())));

        rulesDao.removeTicketAfterTests(createdTicket1);
        rulesDao.removeCustomRuleAfterTests(customRule1);
        rulesDao.removeTicketAfterTests(createdTicket2);
        rulesDao.removeCustomRuleAfterTests(customRule2);
    }

    private CustomRule createCustomRule(String testUser) {
        CustomRule customRule = CustomRuleMapper.toCustomRuleEntity(RulesTestHelper.getCompleteNewCustomRule());
        customRule.setAvailability(AvailabilityType.PUBLIC);
        customRule.setUpdatedBy(testUser);
        RuleSubscription ruleSubscription = new RuleSubscription();
        ruleSubscription.setType(SubscriptionTypeType.TICKET);
        ruleSubscription.setCustomRule(customRule);
        ruleSubscription.setOwner(testUser);
        customRule.getRuleSubscriptionList().add(ruleSubscription);
        customRule = rulesDao.createCustomRule(customRule);
        return customRule;
    }

    private GetTicketListByQueryResponse sendTicketQuery(TicketQuery query){
        GetTicketListByQueryResponse ticketList = getWebTarget()
                .path("tickets/list/" + "testUser")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(query), GetTicketListByQueryResponse.class);

        return ticketList;
    }
}
