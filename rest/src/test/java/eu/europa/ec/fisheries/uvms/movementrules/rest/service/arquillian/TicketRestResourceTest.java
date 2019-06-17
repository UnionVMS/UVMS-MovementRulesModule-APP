package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketSearchKey;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.MRDateUtils;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSubscription;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.CustomRuleMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.TicketMapper;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.inject.Inject;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class TicketRestResourceTest extends BuildRulesRestDeployment {

    @Inject
    private RulesDao rulesDao;

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListByTicketQueryTest() {
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = RulesTestHelper.getTicketListCriteria(TicketSearchKey.RULE_NAME, "Test Name");
        query.getTicketSearchCriteria().add(criteria);

        GetTicketListByQueryResponse ticketList = getWebTarget()
                .path("tickets/list/" + "testUser")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(query), GetTicketListByQueryResponse.class);

        assertNotNull(ticketList.getTickets());
        int numberOfTickets = ticketList.getTickets().size();

        Ticket ticket = RulesTestHelper.getCompleteTicket();

        CustomRule customRule = createCustomRule("testUser");

        ticket.setRuleName(customRule.getName());
        ticket.setRuleGuid(customRule.getGuid().toString());
        ticket.setUpdatedBy("testUser");

        Ticket createdTicket = rulesDao.createTicket(ticket);
        criteria.setValue(customRule.getName());
        ticketList = getWebTarget()
                .path("tickets/list/" + "testUser")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(query), GetTicketListByQueryResponse.class);

        assertThat(ticketList.getTickets().size(), is(numberOfTickets + 1));

        rulesDao.removeTicketAfterTests(createdTicket);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListByTicketQuery_ShouldFailInvalidTestQueryTest() {
        Response response = getWebTarget()
                .path("tickets/list/" + "testUser")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(new TicketQuery()));

        assertEquals(Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void getTicketsByMovementGuidListTest() {
        Response response = getWebTarget()
                .path("tickets/listByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("movement Guid")));
        assertEquals(Status.OK.getStatusCode(), response.getStatus());

        GetTicketListByMovementsResponse ticketList = response.readEntity(GetTicketListByMovementsResponse.class);
        int numberOfPriorTickets = ticketList.getTickets().size();
        assertEquals(0, numberOfPriorTickets);

        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setMovementGuid("movement Guid");
        ticket = rulesDao.createTicket(ticket);

        response = getWebTarget()
                .path("tickets/listByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("movement Guid")));

        ticketList = response.readEntity(GetTicketListByMovementsResponse.class);
        assertEquals(ticketList.getTickets().size(), numberOfPriorTickets + 1);

        rulesDao.removeTicketAfterTests(ticket);
    }

    @Test
    @OperateOnDeployment("normal")
    public void countTicketsByMovementGuidListTest() {
        Long response = getWebTarget()
                .path("tickets/countByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("movement Guid")), Long.class);
        assertEquals((Long)0L, response);

        int numberOfPriorTickets = response.intValue();

        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setMovementGuid("movement Guid");
        ticket = rulesDao.createTicket(ticket);

        response = getWebTarget()
                .path("tickets/countByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("movement Guid")), Long.class);

        assertEquals(response.intValue(), numberOfPriorTickets + 1);
        rulesDao.removeTicketAfterTests(ticket);
    }

    @Test
    @OperateOnDeployment("normal")
    public void updateTicketStatusTest() {
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket = rulesDao.createTicket(ticket);
        TicketType ticketType = TicketMapper.toTicketType(ticket);

        assertEquals(TicketStatusType.OPEN, ticketType.getStatus());

        ticketType.setStatus(TicketStatusType.CLOSED);

        TicketType response = getWebTarget()
                .path("tickets/status")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .put(Entity.json(ticketType), TicketType.class);

        assertEquals(TicketStatusType.CLOSED, response.getStatus());

        rulesDao.removeTicketAfterTests(ticket);
    }

    @Test
    @OperateOnDeployment("normal")
    public void updateTicketStatus_ShouldFailInvalidTicketTest() {
        TicketType ticketType = new TicketType();
        ticketType.setGuid("TestGuid");

        Response response = getWebTarget()
                .path("tickets/status")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .put(Entity.json(ticketType));
        assertEquals(Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void updateTicketStatusByQueryTest() {
        TicketQuery ticketQuery = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria tlc = RulesTestHelper
                .getTicketListCriteria(TicketSearchKey.TICKET_GUID, UUID.randomUUID().toString());
        ticketQuery.getTicketSearchCriteria().add(tlc);

        List<TicketType> response = getWebTarget()
                    .path("tickets/status/")
                    .path("vms_admin_com")
                    .path(TicketStatusType.OPEN.name())
                    .request(MediaType.APPLICATION_JSON)
                    .header(HttpHeaders.AUTHORIZATION, getToken())
                    .post(Entity.json(ticketQuery), new GenericType<List<TicketType>>(){});

        assertEquals(0, response.size());

        Date now = new Date(System.currentTimeMillis() - 1000);
        tlc.setKey(TicketSearchKey.FROM_DATE);
        tlc.setValue(MRDateUtils.dateToString(now.toInstant()));

        CustomRule customRule = createCustomRule("vms_admin_com");

        Ticket ticket1 = RulesTestHelper.getCompleteTicket();
        ticket1.setRuleGuid(customRule.getGuid().toString());
        Ticket ticket2 = RulesTestHelper.getCompleteTicket();
        ticket2.setRuleGuid(customRule.getGuid().toString());
        ticket1 = rulesDao.createTicket(ticket1);
        ticket2 = rulesDao.createTicket(ticket2);

        response = getWebTarget()
                .path("tickets/status/vms_admin_com/" + TicketStatusType.CLOSED)
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(ticketQuery), new GenericType<List<TicketType>>(){});

        assertEquals(2, response.size());

        List<TicketType> filtered = response.stream()
                .filter(tt -> TicketStatusType.CLOSED.equals(tt.getStatus()))
                .collect(Collectors.toList());

        assertEquals(2, filtered.size());

        rulesDao.removeTicketAfterTests(ticket1);
        rulesDao.removeTicketAfterTests(ticket2);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketByGuidTest() {
        Ticket ticket = createTicket();

        TicketType response = getWebTarget()
                .path("tickets/")
                .path(ticket.getGuid().toString())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(TicketType.class);

        assertNotNull(response.getGuid());
        assertEquals(ticket.getGuid().toString(), response.getGuid());

        rulesDao.removeTicketAfterTests(ticket);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketByGuid_ShouldFailInvalidGuidTest() {
        Response response = getWebTarget()
                .path("tickets/")
                .path("INVALID-GUID")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get();
        assertEquals(Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
    }

    @Test
    @OperateOnDeployment("normal")
    public void getNumberOfOpenTicketReportsTest() {
        Long response = getWebTarget()
                .path("/tickets/countopen/")
                .path("invalid_user")    //no tickets in the db
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(Long.class);

        assertEquals(0 , response.intValue());

        CustomRule customRule = createCustomRule("TestUser");

        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleGuid(customRule.getGuid().toString());
        ticket.setUpdatedBy("TestUser");

        response = getWebTarget()
                .path("/tickets/countopen/")
                .path(ticket.getUpdatedBy())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(Long.class);

        int numberOfTicketsBefore = response.intValue();
        ticket = rulesDao.createTicket(ticket);

        response = getWebTarget()
                .path("/tickets/countopen/")
                .path(ticket.getUpdatedBy())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(Long.class);

        assertEquals(numberOfTicketsBefore + 1 , response.intValue());

        rulesDao.removeTicketAfterTests(ticket);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getNumberOfAssetsNotSendingTest() {
        Long response = getWebTarget()
                .path("/tickets/countAssetsNotSending")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(Long.class);

        int numberOfTicketsBefore = response.intValue();

        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleGuid(ServiceConstants.ASSET_NOT_SENDING_RULE);
        rulesDao.createTicket(ticket);

        response = getWebTarget()
                .path("/tickets/countAssetsNotSending")    //no tickets in the db
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(Long.class);

        assertEquals(numberOfTicketsBefore +1 , response.intValue());

        rulesDao.removeTicketAfterTests(ticket);
    }

    private CustomRule createCustomRule(String testUser) {
        CustomRule customRule = CustomRuleMapper.toCustomRuleEntity(RulesTestHelper.getCompleteNewCustomRule());
        customRule.setAvailability(AvailabilityType.GLOBAL);
        customRule.setUpdatedBy(testUser);
        RuleSubscription ruleSubscription = new RuleSubscription();
        ruleSubscription.setType(SubscriptionTypeType.TICKET);
        ruleSubscription.setCustomRule(customRule);
        ruleSubscription.setOwner(testUser);
        customRule.getRuleSubscriptionList().add(ruleSubscription);
        customRule = rulesDao.createCustomRule(customRule);
        return customRule;
    }

    private Ticket createTicket() {
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        return rulesDao.createTicket(ticket);
    }
}
