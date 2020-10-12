package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.*;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.movementrules.ticketrule.v1.TicketAndRuleType;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.CustomRuleListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.TicketListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.*;
import org.jboss.arquillian.container.test.api.OperateOnDeployment;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.ejb.EJBTransactionRolledbackException;
import javax.inject.Inject;
import java.nio.file.AccessDeniedException;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.*;

@RunWith(Arquillian.class)
public class RulesServiceBeanTest extends TransactionalTests {

    @Inject
    private RulesServiceBean rulesService;

    @Inject
    private RulesDao rulesDao;
    
    @Test
    @OperateOnDeployment("normal")
    public void createCustomRuleTest() throws Exception{
        CustomRule input = getCompleteNewCustomRule();
        input.setAvailability(AvailabilityType.GLOBAL.value());

        try {
            rulesService.createCustomRule(input, "test", "test");
            fail();
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }

        try {
            rulesService.createCustomRule(input, "test", null);
            fail();
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }

        input.setAvailability((AvailabilityType.PRIVATE.value()));
        CustomRule output = rulesService.createCustomRule(input, "test", "test");
        Assert.assertNotNull(output.getGuid());
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void createCustomRuleWithIntervalTest() throws Exception {
        CustomRule customRule = getCompleteNewCustomRule();
        Interval interval = new Interval();
        interval.setStart(Instant.now().minus(1, ChronoUnit.HOURS));
        interval.setEnd(Instant.now());
        customRule.getIntervals().add(interval);
        
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        assertThat(createdCustomRule.getGuid(), is(notNullValue()));
        UUID createdIntervalId = createdCustomRule.getIntervals().get(0).getId();
        assertThat(createdIntervalId, is(notNullValue()));
    }

    @Test
    @OperateOnDeployment ("normal")
    public void getCustomRuleByDummyGuidTest() {   //a get with proper input exists among the rest tests
            assertNull(rulesService.getCustomRuleByGuid(UUID.randomUUID()));
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void getCustomRulesByUserTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        List<CustomRule> customRulesByUser = rulesService.getCustomRulesByUser(createdCustomRule.getUpdatedBy());
        assertTrue(customRulesByUser.size() > 0);
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void getRunnableCustomRulesTest() throws Exception {
        List<CustomRule> runnableCustomRulesBefore = rulesService.getRunnableCustomRules();
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        rulesService.createCustomRule(customRule, "", "");

        List<CustomRule> runnableCustomRulesAfter = rulesService.getRunnableCustomRules();
        assertThat(runnableCustomRulesAfter.size(), is(runnableCustomRulesBefore.size() + 1));
    }


    @Test
    @OperateOnDeployment("normal")
    public void getRunnableCustomRulesInactivateRuleTest() throws Exception {
        List<CustomRule> runnableCustomRulesBefore = rulesService.getRunnableCustomRules();
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        List<CustomRule> runnableCustomRulesAfter = rulesService.getRunnableCustomRules();
        assertThat(runnableCustomRulesAfter.size(), is(runnableCustomRulesBefore.size() + 1));

        rulesService.deleteCustomRule(createdCustomRule.getGuid().toString(), "Test", "", "");

        List<CustomRule> runnableCustomRulesAfterDelete = rulesService.getRunnableCustomRules();
        assertThat(runnableCustomRulesAfterDelete.size(), is(runnableCustomRulesBefore.size()));
    }
    
    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleListByQueryGuidTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        CustomRuleQuery query = RulesTestHelper.createBasicCustomRuleQuery();
        CustomRuleListCriteria criteria = new CustomRuleListCriteria();
        criteria.setKey(CustomRuleSearchKey.GUID);
        criteria.setValue(createdCustomRule.getGuid().toString());
        query.getCustomRuleSearchCriteria().add(criteria);

        CustomRuleListResponseDto customRulesResponse = rulesService.getCustomRulesByQuery(query);
        List<CustomRule> customRules = customRulesResponse.getCustomRuleList();

        assertThat(customRules.size(), is(1));

        CustomRule fetchedCustomRule = customRules.get(0);
        assertThat(fetchedCustomRule.getGuid(), is(createdCustomRule.getGuid()));
        assertThat(fetchedCustomRule.getName(), is(createdCustomRule.getName()));
        assertThat(fetchedCustomRule.getUpdatedBy(), is(createdCustomRule.getUpdatedBy()));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleListByQueryGuidTwoGuidsTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        CustomRule customRule2 = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule2 = rulesService.createCustomRule(customRule2, "", "");


        CustomRuleQuery query = RulesTestHelper.createBasicCustomRuleQuery();
        CustomRuleListCriteria criteria = new CustomRuleListCriteria();
        criteria.setKey(CustomRuleSearchKey.GUID);
        criteria.setValue(createdCustomRule.getGuid().toString());
        query.getCustomRuleSearchCriteria().add(criteria);
        CustomRuleListCriteria criteria2 = new CustomRuleListCriteria();
        criteria2.setKey(CustomRuleSearchKey.GUID);
        criteria2.setValue(createdCustomRule2.getGuid().toString());
        query.getCustomRuleSearchCriteria().add(criteria2);


        CustomRuleListResponseDto customRulesResponse = rulesService.getCustomRulesByQuery(query);
        List<CustomRule> customRules = customRulesResponse.getCustomRuleList();

        assertThat(customRules.size(), is(2));
        assertTrue(customRules.stream()
                .anyMatch(r -> r.getGuid().equals(createdCustomRule.getGuid())));
        assertTrue(customRules.stream()
                .anyMatch(r -> r.getGuid().equals(createdCustomRule2.getGuid())));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleListByQueryTicketActionUserTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        String mail = "test@mail.com";
        RuleAction action = new RuleAction();
        action.setAction("EMAIL");
        action.setValue(mail);
        action.setCustomRule(customRule);
        customRule.getRuleActionList().add(action);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        CustomRuleQuery query = RulesTestHelper.createBasicCustomRuleQuery();
        CustomRuleListCriteria criteria = new CustomRuleListCriteria();
        criteria.setKey(CustomRuleSearchKey.TICKET_ACTION_USER);
        criteria.setValue(mail);
        query.getCustomRuleSearchCriteria().add(criteria);

        CustomRuleListResponseDto customRulesResponse = rulesService.getCustomRulesByQuery(query);
        List<CustomRule> customRules = customRulesResponse.getCustomRuleList();

        assertThat(customRules.size(), is(1));

        CustomRule fetchedCustomRule = customRules.get(0);
        assertThat(fetchedCustomRule.getGuid(), is(createdCustomRule.getGuid()));
        assertThat(fetchedCustomRule.getName(), is(createdCustomRule.getName()));
        assertThat(fetchedCustomRule.getUpdatedBy(), is(createdCustomRule.getUpdatedBy()));
    }

    @Test
    @OperateOnDeployment("normal")
    public void getCustomRuleListByQueryUserTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");

        CustomRuleQuery query = RulesTestHelper.createBasicCustomRuleQuery();
        CustomRuleListCriteria criteria = new CustomRuleListCriteria();
        criteria.setKey(CustomRuleSearchKey.RULE_USER);
        criteria.setValue(createdCustomRule.getUpdatedBy());
        query.getCustomRuleSearchCriteria().add(criteria);

        CustomRuleListResponseDto customRulesResponse = rulesService.getCustomRulesByQuery(query);
        List<CustomRule> customRules = customRulesResponse.getCustomRuleList();

        assertTrue(customRules.size() > 0);

        assertTrue(customRules.stream()
                .anyMatch(r -> r.getGuid().equals(createdCustomRule.getGuid())));
    }

    @Test
    @OperateOnDeployment ("normal")
    public void updateDummyCustomRuleTest() throws Exception{      //an update with proper input/execution exists among the rest tests
        CustomRule input = getCompleteNewCustomRule();
        input.setAvailability(AvailabilityType.GLOBAL.value());
        input.setGuid(null);

        //not enough rights
        try {
            rulesService.updateCustomRule(input, "test", "test");
            fail();
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }
        input.setAvailability(AvailabilityType.PRIVATE.value());

        //no guid
        try{
            rulesService.updateCustomRule(input, "test", "test");
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        //null as input, funnily enough they die at different places......
        try{
            rulesService.updateCustomRule(null);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateCustomRule(null, "test", "test");
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        input.setGuid(UUID.randomUUID()); //dummy guid

        userTransaction.rollback();
        userTransaction.begin();

        //no such rule to update
        try{

            rulesService.updateCustomRule(input, "test", "test");
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateCustomRule(input);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void updateCustomRuleTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        String newDescription = "Updated description";
        createdCustomRule.setDescription(newDescription);
        CustomRule updatedCustomRule = rulesService.updateCustomRule(createdCustomRule);
        assertThat(updatedCustomRule.getDescription(), is(newDescription));
        assertNotEquals(updatedCustomRule.getGuid(),createdCustomRule.getGuid());
        assertThat(updatedCustomRule.getRuleSegmentList().size(), is(createdCustomRule.getRuleSegmentList().size()));
    }

    @Test
    @OperateOnDeployment ("normal")
    public void updateCustomRuleCheckRuleSegmentTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        customRule.getRuleSegmentList().clear();
        RuleSegment areaRule = new RuleSegment();
        areaRule.setStartOperator("(");
        areaRule.setCriteria(CriteriaType.AREA.value());
        areaRule.setSubCriteria(SubCriteriaType.AREA_CODE.value());
        areaRule.setCondition(ConditionType.EQ.value());
        areaRule.setValue("DNK");
        areaRule.setEndOperator(")");
        areaRule.setLogicOperator(LogicOperatorType.NONE.value());
        areaRule.setOrder(0);
        areaRule.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(areaRule);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        String newDescription = "Updated description";
        createdCustomRule.setDescription(newDescription);
        CustomRule updatedCustomRule = rulesService.updateCustomRule(createdCustomRule);
        assertThat(updatedCustomRule.getDescription(), is(newDescription));
        assertNotEquals(updatedCustomRule.getGuid(), createdCustomRule.getGuid());
        assertThat(updatedCustomRule.getRuleSegmentList().size(), is(createdCustomRule.getRuleSegmentList().size()));
        assertEquals(updatedCustomRule.getRuleSegmentList().get(0), createdCustomRule.getRuleSegmentList().get(0));
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void updateCustomRuleWithUsernameTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        String newDescription = "Updated description";
        createdCustomRule.setDescription(newDescription);
        CustomRule updatedCustomRule = rulesService.updateCustomRule(createdCustomRule, "", "");
        assertThat(updatedCustomRule.getDescription(), is(newDescription));
        assertNotEquals(updatedCustomRule.getGuid(), createdCustomRule.getGuid());
    }

    @Test
    @OperateOnDeployment ("normal")
    public void deleteCustomRuleWithNullGuidTest() throws Exception {          //a test with proper exectuion exists among the rest tests
        try {
            rulesService.deleteCustomRule(null, "testUser", "testFeature", "testApp");
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void deleteCustomRuleWithDummyGuidTest() throws Exception {
        try {
            rulesService.deleteCustomRule("dummyGuid", "testUser", "testFeature", "testApp");
            fail();
        } catch (EJBTransactionRolledbackException e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    @OperateOnDeployment ("normal")
    public void updateSubscriptionNegativeTests() throws Exception{
        try{
            rulesService.updateSubscription(null, "testUser");
            fail();
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }


        UpdateSubscriptionType input = new UpdateSubscriptionType();
        SubscriptionType subscriptionType = new SubscriptionType();
        input.setSubscription(subscriptionType);

        //incomplete subscriptionType
        try{
            rulesService.updateSubscription(input, "testUser");
            fail();
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        subscriptionType.setType(SubscriptionTypeType.TICKET);
        try{
            rulesService.updateSubscription(input, "testUser");
            fail();
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        subscriptionType.setOwner("tester");

        try{
            rulesService.updateSubscription(input, "testUser"); //no rule Guid
            fail();
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        input.setRuleGuid("dummyGuid");

        userTransaction.rollback();
        userTransaction.begin();

        try{
            rulesService.updateSubscription(input, "testUser"); //non-existant rule guid
            fail();
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }

    @Test
    @OperateOnDeployment ("normal")
    public void updateSubscriptionTest() throws Exception{
        UpdateSubscriptionType input = new UpdateSubscriptionType();
        SubscriptionType subscriptionType = new SubscriptionType();
        input.setSubscription(subscriptionType);
        subscriptionType.setType(SubscriptionTypeType.TICKET);
        subscriptionType.setOwner("tester");

        //create a custom rule and do some actual updates.....
        CustomRule newRule = getCompleteNewCustomRule();
        newRule = rulesService.createCustomRule(newRule, "test", "test");
        Assert.assertNotNull(newRule.getGuid());

        input.setRuleGuid(newRule.getGuid().toString());

        input.setOperation(SubscritionOperationType.ADD);
        CustomRule output = rulesService.updateSubscription(input, null);
        Assert.assertEquals(2, output.getRuleSubscriptionList().size());

        input.setOperation((SubscritionOperationType.REMOVE));
        output = rulesService.updateSubscription(input, null);
        Assert.assertEquals(1, output.getRuleSubscriptionList().size());
        Assert.assertEquals("vms_admin_com", output.getRuleSubscriptionList().get(0).getOwner());
    }


    @Test
    @OperateOnDeployment ("normal")
    public void getTicketListNegativeTest() throws Exception {   //a test with proper input is among the rest tests
        try {
            rulesService.getTicketList(null, null);   //missing query
            fail();
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        TicketQuery input = new TicketQuery();

        try {
            rulesService.getTicketList(null, input);    //missing pagination
            fail();
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void getTicketListByGuidTest() throws Exception {
        String user = "Test user";
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        customRule.setUpdatedBy(user);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        Ticket ticket = getBasicTicket();
        ticket.setRuleGuid(createdCustomRule.getGuid().toString());
        Ticket createdTicket = rulesDao.createTicket(ticket);
        
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(TicketSearchKey.TICKET_GUID);
        criteria.setValue(createdTicket.getGuid().toString());
        query.getTicketSearchCriteria().add(criteria);
        TicketListResponseDto ticketList = rulesService.getTicketList(user, query);
        List<Ticket> tickets = ticketList.getTicketList();
        assertThat(tickets.size(), is(1));
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void getTicketListByGuidTwoTicketsTest() throws Exception {
        String user = "Test user";
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        customRule.setUpdatedBy(user);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        Ticket ticket = getBasicTicket();
        ticket.setRuleGuid(createdCustomRule.getGuid().toString());
        Ticket createdTicket = rulesDao.createTicket(ticket);
        
        Ticket ticket2 = getBasicTicket();
        ticket2.setRuleGuid(createdCustomRule.getGuid().toString());
        Ticket createdTicket2 = rulesDao.createTicket(ticket2);
        
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(TicketSearchKey.TICKET_GUID);
        criteria.setValue(createdTicket.getGuid().toString());
        query.getTicketSearchCriteria().add(criteria);
        TicketListCriteria criteria2 = new TicketListCriteria();
        criteria2.setKey(TicketSearchKey.TICKET_GUID);
        criteria2.setValue(createdTicket2.getGuid().toString());
        query.getTicketSearchCriteria().add(criteria2);
        TicketListResponseDto ticketList = rulesService.getTicketList(user, query);
        List<Ticket> tickets = ticketList.getTicketList();
        assertThat(tickets.size(), is(2));
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void getTicketListByAssetGuidTest() throws Exception {
        String user = "Test user";
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        customRule.setUpdatedBy(user);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        Ticket ticket = getBasicTicket();
        ticket.setRuleGuid(createdCustomRule.getGuid().toString());
        Ticket createdTicket = rulesDao.createTicket(ticket);
        
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(TicketSearchKey.ASSET_GUID);
        criteria.setValue(createdTicket.getAssetGuid());
        query.getTicketSearchCriteria().add(criteria);
        TicketListResponseDto ticketList = rulesService.getTicketList(user, query);
        List<Ticket> tickets = ticketList.getTicketList();
        assertThat(tickets.size(), is(1));
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void getTicketListByRuleGuidTest() throws Exception {
        String user = "Test user";
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        customRule.setUpdatedBy(user);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        Ticket ticket = getBasicTicket();
        ticket.setRuleGuid(createdCustomRule.getGuid().toString());
        rulesDao.createTicket(ticket);
        
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(TicketSearchKey.RULE_GUID);
        criteria.setValue(createdCustomRule.getGuid().toString());
        query.getTicketSearchCriteria().add(criteria);
        TicketListResponseDto ticketList = rulesService.getTicketList(user, query);
        List<Ticket> tickets = ticketList.getTicketList();
        assertThat(tickets.size(), is(1));
    }

    @Test
    @OperateOnDeployment ("normal")
    public void getTicketsByMovementsNegativeTest() throws Exception{ //a test with proper input is among the rest tests
        try{
            rulesService.getTicketsByMovements(null);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        List<String> input = new ArrayList<>();

        try{
            rulesService.getTicketsByMovements(input);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void getTicketsByMovementsTest() throws Exception {
        Ticket ticket = getBasicTicket();
        String movementGuid = UUID.randomUUID().toString();
        ticket.setMovementGuid(movementGuid);
        rulesDao.createTicket(ticket);
        
        List<Ticket> ticketsByMovements = rulesService.getTicketsByMovements(Collections.singletonList(movementGuid));
        assertThat(ticketsByMovements.size(), is(1));
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void getTicketsAndRulesByMovementsEmptyTest() throws Exception {
        GetTicketsAndRulesByMovementsResponse response = rulesService.getTicketsAndRulesByMovements(Collections.singletonList(""));
        List<TicketAndRuleType> ticketsAndRules = response.getTicketsAndRules();
        assertThat(ticketsAndRules.size(), is(0));
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void getTicketsAndRulesByMovementsTest() throws Exception {
        String guid = UUID.randomUUID().toString();
        
        CustomRule customRule = getCompleteNewCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        Ticket ticket = getBasicTicket();
        ticket.setRuleGuid(createdCustomRule.getGuid().toString());
        ticket.setMovementGuid(guid);
        rulesDao.createTicket(ticket);
        
        GetTicketsAndRulesByMovementsResponse response = rulesService.getTicketsAndRulesByMovements(Collections.singletonList(guid));
        List<TicketAndRuleType> ticketsAndRules = response.getTicketsAndRules();
        assertThat(ticketsAndRules.size(), is(1));
        assertThat(ticketsAndRules.get(0).getRule().getGuid(), is(createdCustomRule.getGuid().toString()));
    }

    @Test
    @OperateOnDeployment ("normal")
    public void countTicketsByMovementNegativeTest() throws Exception { //a test with proper input is among the rest tests
        try{
            rulesService.getTicketsByMovements(null);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        List<String> input = new ArrayList<>();

        try{
            rulesService.getTicketsByMovements(input);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void countTicketsByMovementTest() throws Exception {
        Ticket ticket = getBasicTicket();
        String movementGuid = UUID.randomUUID().toString();
        ticket.setMovementGuid(movementGuid);
        rulesDao.createTicket(ticket);
        
        long ticketsByMovements = rulesService.countTicketsByMovements(Collections.singletonList(movementGuid));
        assertThat(ticketsByMovements, is(1L));
    }

    @Test
    @OperateOnDeployment ("normal")
    public void updateTicketStatusNegativeTest() throws Exception {     //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketStatus(null);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        Ticket input = new Ticket();
        try{
            rulesService.updateTicketStatus(input);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void updateTicketStatusTest() throws Exception {
        Ticket ticket = getBasicTicket();
        Ticket createdTicket = rulesDao.createTicket(ticket);
        TicketStatusType newStatus = TicketStatusType.POLL_PENDING;
        createdTicket.setStatus(newStatus);
        
        Ticket updatedTicket = rulesService.updateTicketStatus(createdTicket);
        assertThat(updatedTicket.getStatus(), is(newStatus));
        assertThat(updatedTicket.getGuid(), is(createdTicket.getGuid()));
    }

    @Test
    @OperateOnDeployment ("normal")
    public void updateTicketStatusByQueryNegativeTest() throws Exception{   //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketStatusByQuery(null, null, null);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateTicketStatusByQuery("test user", null, null);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateTicketStatusByQuery("test user", null, TicketStatusType.OPEN);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        TicketQuery input = new TicketQuery();
        input.getTicketSearchCriteria().add(new TicketListCriteria());
        try{
            rulesService.updateTicketStatusByQuery("test user", input, TicketStatusType.OPEN);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    @OperateOnDeployment ("normal")
    public void updateTicketStatusByQueryTest() throws Exception {
        String user = "Test user";
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        customRule.setUpdatedBy(user);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        Ticket ticket = getBasicTicket();
        ticket.setRuleGuid(createdCustomRule.getGuid().toString());
        ticket.setStatus(TicketStatusType.OPEN);
        Ticket createdTicket = rulesDao.createTicket(ticket);
        
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(TicketSearchKey.TICKET_GUID);
        criteria.setValue(createdTicket.getGuid().toString());
        query.getTicketSearchCriteria().add(criteria);
        
        rulesService.updateTicketStatusByQuery(user, query, TicketStatusType.POLL_PENDING);
        
        Ticket fetchedTicket = rulesService.getTicketByGuid(createdTicket.getGuid());
        assertThat(fetchedTicket.getStatus(), is(TicketStatusType.POLL_PENDING));
        assertThat(fetchedTicket.getGuid(), is(createdTicket.getGuid()));
    }



    @Test
    @OperateOnDeployment ("normal")
    public void updateTicketCountNegativeTest() {     //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketCount(null);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        Ticket input = new Ticket();
        try{
            rulesService.updateTicketCount(input);
            fail();
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }

    //getAlarmReportByGuid and getTicketByGuid have tests among the rest tests

    private CustomRule getCompleteNewCustomRule(){
        CustomRule customRule = new CustomRule();

        customRule.setName("Flag SWE && area DNK => Send to DNK" + " (" + System.currentTimeMillis() + ")");
        customRule.setAvailability(AvailabilityType.PRIVATE.value());
        customRule.setUpdatedBy("vms_admin_com");
        customRule.setActive(true);
        customRule.setArchived(false);

        // If flagstate = SWE
        RuleSegment ruleSegment = new RuleSegment();
        ruleSegment.setStartOperator("(");
        ruleSegment.setCriteria(CriteriaType.ASSET.value());
        ruleSegment.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        ruleSegment.setCondition(ConditionType.EQ.value());
        ruleSegment.setValue("SWE");
        ruleSegment.setEndOperator(")");
        ruleSegment.setLogicOperator(LogicOperatorType.AND.value());
        ruleSegment.setOrder(0);
        ruleSegment.setCustomRule(customRule);

        customRule.getRuleSegmentList().add(ruleSegment);

        // and area = DNK
        CustomRuleSegmentType areaRule = new CustomRuleSegmentType();
        areaRule.setStartOperator("(");
        areaRule.setCriteria(CriteriaType.AREA);
        areaRule.setSubCriteria(SubCriteriaType.AREA_CODE);
        areaRule.setCondition(ConditionType.EQ);
        areaRule.setValue("DNK");
        areaRule.setEndOperator(")");
        areaRule.setLogicBoolOperator(LogicOperatorType.NONE);
        areaRule.setOrder("1");

        ruleSegment = new RuleSegment();

        ruleSegment.setStartOperator("(");
        ruleSegment.setCriteria(CriteriaType.AREA.value());
        ruleSegment.setSubCriteria(SubCriteriaType.AREA_CODE.value());
        ruleSegment.setCondition(ConditionType.EQ.value());
        ruleSegment.setValue("DNK");
        ruleSegment.setEndOperator(")");
        ruleSegment.setLogicOperator(LogicOperatorType.NONE.value());
        ruleSegment.setOrder(1);
        ruleSegment.setCustomRule(customRule);

        customRule.getRuleSegmentList().add(ruleSegment);

        // then send to FLUX DNK
        CustomRuleActionType action = new CustomRuleActionType();
        action.setAction(ActionType.SEND_REPORT);
        action.setTarget("FLUX");
        action.setValue("FLUX DNK");
        action.setOrder("0");

        RuleAction ruleAction = new RuleAction();
        ruleAction.setAction(ActionType.SEND_REPORT.value());
        ruleAction.setTarget("FLUX");
        ruleAction.setValue("FLUX DNK");
        ruleAction.setOrder(0);
        ruleAction.setCustomRule(customRule);

        customRule.getRuleActionList().add(ruleAction);

        return customRule;
    }
    
    private Ticket getBasicTicket() {
        Ticket ticket = new Ticket();
        ticket.setAssetGuid(UUID.randomUUID().toString());
        ticket.setStatus(TicketStatusType.OPEN);
        ticket.setCreatedDate(Instant.now());
        ticket.setRuleName("Test Rule");
        ticket.setUpdated(Instant.now());
        ticket.setUpdatedBy("Test user");
        return ticket;
    }
}
