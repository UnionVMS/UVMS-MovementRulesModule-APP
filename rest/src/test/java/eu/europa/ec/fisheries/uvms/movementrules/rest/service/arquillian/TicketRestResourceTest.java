package eu.europa.ec.fisheries.uvms.movementrules.rest.service.arquillian;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.ListPagination;
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
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import java.util.*;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

//@RunAsClient
@RunWith(Arquillian.class)
public class TicketRestResourceTest extends BuildRulesRestDeployment {

    @Inject
    private RulesDao rulesDao;

    @Test
    @OperateOnDeployment("normal")
    public void getTicketListTest() throws Exception {
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(TicketSearchKey.RULE_NAME);
        criteria.setValue("Test Name");
        query.getTicketSearchCriteria().add(criteria);
        
        String response = getWebTarget()
                .path("tickets/list/" + "testUser")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(query), String.class);
        
        GetTicketListByQueryResponse ticketList = RulesTestHelper.deserializeResponseDto(response, GetTicketListByQueryResponse.class);
        assertNotNull(ticketList.getTickets());
        int numberOfTickets = ticketList.getTickets().size();

        Ticket ticket = RulesTestHelper.getCompleteTicket();

        CustomRule customRule = CustomRuleMapper.toCustomRuleEntity(RulesTestHelper.getCompleteNewCustomRule());
        customRule.setAvailability(AvailabilityType.GLOBAL);
        customRule.setUpdatedBy("testUser");
        RuleSubscription ruleSubscription = new RuleSubscription();
        ruleSubscription.setType(SubscriptionTypeType.TICKET);
        ruleSubscription.setCustomRule(customRule);
        ruleSubscription.setOwner("testUser");
        customRule.getRuleSubscriptionList().add(ruleSubscription);
        customRule = rulesDao.createCustomRule(customRule);

        ticket.setRuleName(customRule.getName());
        ticket.setRuleGuid(customRule.getGuid().toString());
        ticket.setUpdatedBy("testUser");

        Ticket createdTicket = rulesDao.createTicket(ticket);
        criteria.setValue(customRule.getName());
        response = getWebTarget()
                .path("tickets/list/" + "testUser")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(query), String.class);

        ticketList = RulesTestHelper.deserializeResponseDto(response, GetTicketListByQueryResponse.class);
        assertThat(ticketList.getTickets().size(), is(numberOfTickets + 1));

        rulesDao.removeTicketAfterTests(createdTicket);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    @Test
    @OperateOnDeployment("normal")
    public void negativeGetTicketListTest() throws Exception{
        String response = getWebTarget()
                .path("tickets/list/" + "testUser")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(new TicketQuery()), String.class);

        assertEquals(500, getReturnCode(response));
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void getTicketsByMovementsTest() throws Exception {
        String response = getWebTarget()
                .path("tickets/listByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("TEST_GUID")), String.class);
        
        GetTicketListByMovementsResponse ticketList = RulesTestHelper.deserializeResponseDto(response, GetTicketListByMovementsResponse.class);
        assertThat(ticketList.getTickets().size(), is(0));

        response = getWebTarget()
                .path("tickets/listByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("movement Guid")), String.class);

        ticketList = RulesTestHelper.deserializeResponseDto(response, GetTicketListByMovementsResponse.class);
        assertNotNull(ticketList.getTickets());
        int numberOfPriorTickets = ticketList.getTickets().size();


        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setMovementGuid("movement Guid");
        ticket = rulesDao.createTicket(ticket);

        response = getWebTarget()
                .path("tickets/listByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("movement Guid")), String.class);
        ticketList = RulesTestHelper.deserializeResponseDto(response, GetTicketListByMovementsResponse.class);
        assertEquals(ticketList.getTickets().size(), numberOfPriorTickets + 1);

        rulesDao.removeTicketAfterTests(ticket);
    }

    @Test
    @OperateOnDeployment("normal")
    public void countTicketsByMovementsTest() throws Exception {
        String response = getWebTarget()
                .path("tickets/countByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("TEST_GUID")), String.class);
        
        Long ticketList = RulesTestHelper.deserializeResponseDto(response, Long.class);
        assertThat(ticketList, is(0L));

        response = getWebTarget()
                .path("tickets/countByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("movement Guid")), String.class);

        ticketList = RulesTestHelper.deserializeResponseDto(response, Long.class);
        assertNotNull(ticketList);
        int numberOfPriorTickets = ticketList.intValue();


        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setMovementGuid("movement Guid");
        ticket = rulesDao.createTicket(ticket);

        response = getWebTarget()
                .path("tickets/countByMovements")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .post(Entity.json(Collections.singletonList("movement Guid")), String.class);
        ticketList = RulesTestHelper.deserializeResponseDto(response, Long.class);
        assertEquals(ticketList.intValue(), numberOfPriorTickets + 1);

        rulesDao.removeTicketAfterTests(ticket);
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void negativeUpdateTicketStatusTest() throws Exception{ //there are no tickets to update so this one will result in an internal server error
        TicketType ticketType = new TicketType();
        ticketType.setGuid("TestGuid");

        String response = getWebTarget()
                .path("tickets/status")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .put(Entity.json(ticketType), String.class);
        assertEquals(500 ,getReturnCode(response));
    }

    @Test
    @OperateOnDeployment("normal")
    public void updateTicketStatusTest() throws Exception{
        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setStatus(TicketStatusType.OPEN);
        ticket = rulesDao.createTicket(ticket);
        TicketType ticketType = TicketMapper.toTicketType(ticket);
        ticketType.setStatus(TicketStatusType.CLOSED);

        String response = getWebTarget()
                .path("tickets/status")
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .put(Entity.json(ticketType), String.class);
        TicketType responseTicket = RulesTestHelper.deserializeResponseDto(response, TicketType.class);

        assertEquals(TicketStatusType.CLOSED, responseTicket.getStatus());

        rulesDao.removeTicketAfterTests(ticket);
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void updateTicketStatusByQueryTest() throws Exception {
        TicketQuery ticketQuery = new TicketQuery();
        TicketListCriteria tlc = new TicketListCriteria();
        tlc.setKey(TicketSearchKey.TICKET_GUID);
        tlc.setValue(UUID.randomUUID().toString());
        ticketQuery.getTicketSearchCriteria().add(tlc);
        ListPagination lp = new ListPagination();
        lp.setPage(1);
        lp.setListSize(10);
        ticketQuery.setPagination(lp);


        String response = getWebTarget()
                    .path("tickets/status/vms_admin_com/" + TicketStatusType.OPEN)
                    .request(MediaType.APPLICATION_JSON)
                    .header(HttpHeaders.AUTHORIZATION, getToken())
                    .post(Entity.json(ticketQuery), String.class);

        //ObjectMapper objectMapper = new ObjectMapper();
        //why are we treating this as an array??????
        //Object[] returnArray = deserializeResponseDto(response, Object[].class);
        //I have no idea why but for some reason the response gets deserialized as a hashmap instead of a ticketType, so bring out the ugly hack......
        //List<TicketType> responseList = deserializeResponseDto(response, List.class);
        List<HashMap> responseList = RulesTestHelper.deserializeResponseDto(response, List.class);
        assertThat(responseList.size(), is(0));

        Date now = new Date(System.currentTimeMillis() - 1000);
        tlc.setKey(TicketSearchKey.FROM_DATE);
        tlc.setValue(MRDateUtils.dateToString(now.toInstant()));

        CustomRule customRule = CustomRuleMapper.toCustomRuleEntity(RulesTestHelper.getCompleteNewCustomRule());
        customRule.setAvailability(AvailabilityType.GLOBAL);
        customRule.setUpdatedBy("vms_admin_com");
        RuleSubscription ruleSubscription = new RuleSubscription();
        ruleSubscription.setType(SubscriptionTypeType.TICKET);
        ruleSubscription.setCustomRule(customRule);
        ruleSubscription.setOwner("vms_admin_com");
        customRule.getRuleSubscriptionList().add(ruleSubscription);
        customRule = rulesDao.createCustomRule(customRule);

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
                .post(Entity.json(ticketQuery), String.class);

        //ObjectMapper objectMapper = new ObjectMapper();
        responseList = RulesTestHelper.deserializeResponseDto(response, ArrayList.class);
        assertEquals(2 ,responseList.size());
        //Why is it treating this as a hash map???????
        for(HashMap ticketType : responseList){
            assertEquals(TicketStatusType.CLOSED.value(), ticketType.get("status"));
        }

        rulesDao.removeTicketAfterTests(ticket1);
        rulesDao.removeTicketAfterTests(ticket2);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getTicketByGuid() throws Exception {
        String response = getWebTarget()
                .path("tickets/" + "TestGuid")    //no tickets in the db
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(String.class);
        assertEquals(500, getReturnCode(response));


        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket = rulesDao.createTicket(ticket);

        response = getWebTarget()
                .path("tickets/" + ticket.getGuid())    //no tickets in the db
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(String.class);

        TicketType responseTicket = RulesTestHelper.deserializeResponseDto(response, TicketType.class);
        assertNotNull(responseTicket);
        assertEquals(ticket.getGuid().toString(), responseTicket.getGuid());

        rulesDao.removeTicketAfterTests(ticket);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getNumberOfOpenTicketReportsTest() throws Exception{
        String response = getWebTarget()
                .path("/tickets/countopen/" + "ShouldBeEmpty")    //no tickets in the db
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(String.class);

        Long ticketList = RulesTestHelper.deserializeResponseDto(response, Long.class);
        assertEquals(0 , ticketList.intValue());

        CustomRule customRule = CustomRuleMapper.toCustomRuleEntity(RulesTestHelper.getCompleteNewCustomRule());
        customRule.setAvailability(AvailabilityType.GLOBAL);
        customRule.setUpdatedBy("TestUser");
        RuleSubscription ruleSubscription = new RuleSubscription();
        ruleSubscription.setType(SubscriptionTypeType.TICKET);
        ruleSubscription.setCustomRule(customRule);
        ruleSubscription.setOwner("TestUser");
        customRule.getRuleSubscriptionList().add(ruleSubscription);
        customRule = rulesDao.createCustomRule(customRule);

        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleGuid(customRule.getGuid().toString());
        ticket.setUpdatedBy("TestUser");
        response = getWebTarget()
                .path("/tickets/countopen/" + ticket.getUpdatedBy())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(String.class);

        ticketList = RulesTestHelper.deserializeResponseDto(response, Long.class);
        assertNotNull(ticketList);
        int numberOfTicketsB4 = ticketList.intValue();
        ticket = rulesDao.createTicket(ticket);

        response = getWebTarget()
                .path("/tickets/countopen/" + ticket.getUpdatedBy())
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(String.class);

        ticketList = RulesTestHelper.deserializeResponseDto(response, Long.class);
        assertNotNull(ticketList);
        assertEquals(numberOfTicketsB4 + 1 , ticketList.intValue());

        rulesDao.removeTicketAfterTests(ticket);
        rulesDao.removeCustomRuleAfterTests(customRule);
    }

    @Test
    @OperateOnDeployment("normal")
    public void getNumberOfAssetsNotSendingTest() throws Exception{
        String response = getWebTarget()
                .path("/tickets/countAssetsNotSending")    //no tickets in the db
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(String.class);

        Long ticketList = RulesTestHelper.deserializeResponseDto(response, Long.class);
        assertNotNull(ticketList);
        int numberOfTicketsB4 = ticketList.intValue();

        Ticket ticket = RulesTestHelper.getCompleteTicket();
        ticket.setRuleGuid(ServiceConstants.ASSET_NOT_SENDING_RULE);
        rulesDao.createTicket(ticket);

        response = getWebTarget()
                .path("/tickets/countAssetsNotSending")    //no tickets in the db
                .request(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.AUTHORIZATION, getToken())
                .get(String.class);

        ticketList = RulesTestHelper.deserializeResponseDto(response, Long.class);
        assertEquals(numberOfTicketsB4 +1 , ticketList.intValue());

        rulesDao.removeTicketAfterTests(ticket);
    }

    private ObjectMapper objectMapper = new ObjectMapper();
    private int getReturnCode(String responsDto) throws Exception{
        return objectMapper.readValue(responsDto, ObjectNode.class).get("code").asInt();
    }

}
