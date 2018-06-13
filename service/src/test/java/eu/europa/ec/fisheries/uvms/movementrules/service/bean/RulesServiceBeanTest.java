package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import java.nio.file.AccessDeniedException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import javax.ejb.EJBTransactionRolledbackException;
import javax.inject.Inject;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.movementrules.alarm.v1.AlarmReportType;
import eu.europa.ec.fisheries.schema.movementrules.alarm.v1.AlarmStatusType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.ActionType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.ConditionType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CriteriaType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CustomRuleActionType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CustomRuleSegmentType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.LogicOperatorType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubCriteriaType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.SubscritionOperationType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.UpdateSubscriptionType;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.AlarmSearchKey;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketSearchKey;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.movementrules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.AlarmListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.TicketListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.AlarmReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Interval;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RawMovement;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleAction;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.RuleSegment;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.NoEntityFoundException;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;


@RunWith(Arquillian.class)
public class RulesServiceBeanTest extends TransactionalTests {

    @Inject
    RulesService rulesService;

    @Inject
    RulesDao rulesDao;
    
    @Test
    public void createCustomRuleTest() throws Exception{
        CustomRule input = getCompleteNewCustomRule();
        input.setAvailability(AvailabilityType.GLOBAL.value());

        CustomRule output = null;
        try {
            output = rulesService.createCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }

        try {
            output = rulesService.createCustomRule(input, "test", null);
            Assert.assertTrue(false);
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }

        try {
            output = rulesService.createCustomRule(input, null, "test");
            Assert.assertTrue(false);
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }

        input.setAvailability((AvailabilityType.PRIVATE.value()));
        output = rulesService.createCustomRule(input, "test", "test");
        Assert.assertNotNull(output.getGuid());

    }
    
    @Test
    public void createCustomRuleWithIntervalTest() throws Exception {
        CustomRule customRule = getCompleteNewCustomRule();
        Interval interval = new Interval();
        Calendar calendar = Calendar.getInstance();
        calendar.add(Calendar.HOUR, -1);
        interval.setStart(calendar.getTime());
        interval.setEnd(new Date());
        customRule.getIntervals().add(interval);
        
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        assertThat(createdCustomRule.getGuid(), is(notNullValue()));
        Long createdIntervalId = createdCustomRule.getIntervals().get(0).getId();
        assertThat(createdIntervalId, is(notNullValue()));
    }

    @Test
    public void getCustomRuleByDummyGuidTest() throws Exception{   //a get with proper input exists among the rest tests
        try {
            rulesService.getCustomRuleByGuid("dummyGuid");
            Assert.assertTrue(false);
        }catch (RulesServiceException e){
            Assert.assertTrue(true);
        }

    }

    @Test
    public void updateDummyCustomRuleTest() throws Exception{      //an update with proper input/execution exists among the rest tests
        CustomRule input = getCompleteNewCustomRule();
        input.setAvailability(AvailabilityType.GLOBAL.value());
        input.setGuid(null);

        //not enough rights
        try {
            rulesService.updateCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }
        input.setAvailability(AvailabilityType.PRIVATE.value());

        //no guid
        try{
            rulesService.updateCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        //null as input, funnily enough they die at different places......
        try{
            rulesService.updateCustomRule(null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateCustomRule(null, "test", "test");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        input.setGuid("dummyGuid");

        userTransaction.rollback();
        userTransaction.begin();

        //no such rule to update
        try{

            rulesService.updateCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch(NoEntityFoundException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateCustomRule(input);
            Assert.assertTrue(false);
        }catch(NoEntityFoundException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    public void updateCustomRuleTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        String newDescription = "Updated description";
        createdCustomRule.setDescription(newDescription);
        CustomRule updatedCustomRule = rulesService.updateCustomRule(createdCustomRule);
        assertThat(updatedCustomRule.getDescription(), is(newDescription));
        assertThat(updatedCustomRule.getGuid(), is(createdCustomRule.getGuid()));
    }
   
    @Test
    public void updateCustomRuleWithUsernameTest() throws Exception {
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        String newDescription = "Updated description";
        createdCustomRule.setDescription(newDescription);
        CustomRule updatedCustomRule = rulesService.updateCustomRule(createdCustomRule, "", "");
        assertThat(updatedCustomRule.getDescription(), is(newDescription));
        assertThat(updatedCustomRule.getGuid(), is(createdCustomRule.getGuid()));
    }

    @Test
    public void deleteCustomRuleWithNullGuidTest() throws Exception {          //a test with proper exectuion exists among the rest tests
        try {
            rulesService.deleteCustomRule(null, "testUser", "testFeature", "testApp");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    public void deleteCustomRuleWithDummyGuidTest() throws Exception {
        try {
            rulesService.deleteCustomRule("dummyGuid", "testUser", "testFeature", "testApp");
            Assert.assertTrue(false);
        } catch (RulesServiceException e) {
            Assert.assertTrue(true);
        }
    }

    @Test
    public void updateSubscriptionNegativeTests() throws Exception{
        try{
            rulesService.updateSubscription(null, "testUser");
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }


        UpdateSubscriptionType input = new UpdateSubscriptionType();
        SubscriptionType subscriptionType = new SubscriptionType();
        input.setSubscription(subscriptionType);

        //incomplete subscriptionType
        try{
            rulesService.updateSubscription(input, "testUser");
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        subscriptionType.setType(SubscriptionTypeType.TICKET);
        try{
            rulesService.updateSubscription(input, "testUser");
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        subscriptionType.setOwner("tester");

        try{
            rulesService.updateSubscription(input, "testUser"); //no rule Guid
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        input.setRuleGuid("dummyGuid");

        userTransaction.rollback();
        userTransaction.begin();

        try{
            rulesService.updateSubscription(input, "testUser"); //non-existant rule guid
            Assert.assertTrue(false);
        }catch (NoEntityFoundException e){
            Assert.assertTrue(true);
        }

    }

    @Test
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

        input.setRuleGuid(newRule.getGuid());

        input.setOperation(SubscritionOperationType.ADD);
        CustomRule output = rulesService.updateSubscription(input, null);
        Assert.assertEquals(2, output.getRuleSubscriptionList().size());

        input.setOperation((SubscritionOperationType.REMOVE));
        output = rulesService.updateSubscription(input, null);
        Assert.assertEquals(1, output.getRuleSubscriptionList().size());
        Assert.assertEquals("vms_admin_com", output.getRuleSubscriptionList().get(0).getOwner());
    }

    @Test
    public void getAlarmListTest() throws Exception {
        AlarmReport alarmReport = getBasicAlarmReport();
        AlarmReport createdAlarmReport = rulesDao.createAlarmReport(alarmReport);
        
        AlarmQuery query = RulesTestHelper.getBasicAlarmQuery();
        AlarmListCriteria criteria = new AlarmListCriteria();
        criteria.setKey(AlarmSearchKey.ALARM_GUID);
        criteria.setValue(createdAlarmReport.getGuid());
        query.getAlarmSearchCriteria().add(criteria);
        
        AlarmListResponseDto alarmList = rulesService.getAlarmList(query);
        List<AlarmReportType> alarms = alarmList.getAlarmList();
        
        assertThat(alarms.size(), is(1));
        assertThat(alarms.get(0).getGuid(), is(createdAlarmReport.getGuid()));
    }

    @Test
    public void getTicketListNegativeTest() throws Exception {   //a test with proper input is among the rest tests
        try {
            rulesService.getTicketList(null, null);   //missing query
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        TicketQuery input = new TicketQuery();

        try {
            rulesService.getTicketList(null, input);    //missing pagination
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    public void getTicketListTest() throws Exception {
        String user = "Test user";
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        customRule.setUpdatedBy(user);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        Ticket ticket = getBasicTicket();
        ticket.setRuleGuid(createdCustomRule.getGuid());
        Ticket createdTicket = rulesDao.createTicket(ticket);
        
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(TicketSearchKey.TICKET_GUID);
        criteria.setValue(createdTicket.getGuid());
        query.getTicketSearchCriteria().add(criteria);
        TicketListResponseDto ticketList = rulesService.getTicketList(user, query);
        List<Ticket> tickets = ticketList.getTicketList();
        assertThat(tickets.size(), is(1));
    }

    @Test
    public void getTicketsByMovementsNegativeTest() throws Exception{ //a test with proper input is among the rest tests
        try{
            rulesService.getTicketsByMovements(null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        List<String> input = new ArrayList<String>();

        try{
            rulesService.getTicketsByMovements(input);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    public void getTicketsByMovementsTest() throws Exception {
        Ticket ticket = getBasicTicket();
        String movementGuid = UUID.randomUUID().toString();
        ticket.setMovementGuid(movementGuid);
        rulesDao.createTicket(ticket);
        
        List<Ticket> ticketsByMovements = rulesService.getTicketsByMovements(Arrays.asList(movementGuid));
        assertThat(ticketsByMovements.size(), is(1));
    }

    @Test
    public void countTicketsByMovementNegativeTest() throws Exception { //a test with proper input is among the rest tests
        try{
            rulesService.getTicketsByMovements(null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        List<String> input = new ArrayList<String>();

        try{
            rulesService.getTicketsByMovements(input);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    public void countTicketsByMovementTest() throws Exception {
        Ticket ticket = getBasicTicket();
        String movementGuid = UUID.randomUUID().toString();
        ticket.setMovementGuid(movementGuid);
        rulesDao.createTicket(ticket);
        
        long ticketsByMovements = rulesService.countTicketsByMovements(Arrays.asList(movementGuid));
        assertThat(ticketsByMovements, is(1L));
    }

    @Test
    public void updateTicketStatusNegativeTest() throws Exception {     //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketStatus(null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        Ticket input = new Ticket();
        try{
            rulesService.updateTicketStatus(input);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    public void updateTicketStatusTest() throws Exception {
        Ticket ticket = getBasicTicket();
        Ticket createdTicket = rulesDao.createTicket(ticket);
        String newStatus = "New status";
        createdTicket.setStatus(newStatus);
        
        Ticket updatedTicket = rulesService.updateTicketStatus(createdTicket);
        assertThat(updatedTicket.getStatus(), is(newStatus));
        assertThat(updatedTicket.getGuid(), is(createdTicket.getGuid()));
    }

    @Test
    public void updateTicketStatusByQueryNegativeTest() throws Exception{   //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketStatusByQuery(null, null, null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateTicketStatusByQuery("test user", null, null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateTicketStatusByQuery("test user", null, TicketStatusType.OPEN);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
        TicketQuery input = new TicketQuery();
        input.getTicketSearchCriteria().add(new TicketListCriteria());
        try{
            rulesService.updateTicketStatusByQuery("test user", input, TicketStatusType.OPEN);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    public void updateTicketStatusByQueryTest() throws Exception {
        String user = "Test user";
        CustomRule customRule = RulesTestHelper.createCompleteCustomRule();
        customRule.setUpdatedBy(user);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        Ticket ticket = getBasicTicket();
        ticket.setRuleGuid(createdCustomRule.getGuid());
        ticket.setStatus(TicketStatusType.OPEN.value());
        Ticket createdTicket = rulesDao.createTicket(ticket);
        
        TicketQuery query = RulesTestHelper.getBasicTicketQuery();
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(TicketSearchKey.TICKET_GUID);
        criteria.setValue(createdTicket.getGuid());
        query.getTicketSearchCriteria().add(criteria);
        
        rulesService.updateTicketStatusByQuery(user, query, TicketStatusType.PENDING);
        
        Ticket fetchedTicket = rulesService.getTicketByGuid(createdTicket.getGuid());
        assertThat(fetchedTicket.getStatus(), is(TicketStatusType.PENDING.value()));
        assertThat(fetchedTicket.getGuid(), is(createdTicket.getGuid()));
    }

    @Test
    public void updateAlarmStatusNegativeTest() throws Exception{ //a test with proper input is among the rest tests
        AlarmReport input = new AlarmReport();
        try{
            rulesService.updateAlarmStatus(input);
            Assert.assertTrue(false);
        }catch(NoEntityFoundException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    public void updateAlarmStatusTest() throws Exception {
        AlarmReport alarmReport = getBasicAlarmReport();
        AlarmReport createdAlarmReport = rulesDao.createAlarmReport(alarmReport);
        String newStatus = "New status";
        createdAlarmReport.setStatus(newStatus);
        
        AlarmReport updatedAlarmReport = rulesService.updateAlarmStatus(createdAlarmReport);
        assertThat(updatedAlarmReport.getStatus(), is(newStatus));
        assertThat(updatedAlarmReport.getGuid(), is(createdAlarmReport.getGuid()));
    }

    @Test
    public void updateTicketCountNegativeTest() throws Exception {     //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketCount(null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        Ticket input = new Ticket();
        try{
            rulesService.updateTicketCount(input);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }
    
    @Test
    public void reprocessAlarmTest() throws Exception {
        AlarmReport alarm1 = getBasicAlarmReport();
        alarm1.setRawMovement(getBasicRawMovement());
        AlarmReport createdAlarm1 = rulesDao.createAlarmReport(alarm1);
        AlarmReport alarm2 = getBasicAlarmReport();
        alarm2.setRawMovement(getBasicRawMovement());
        AlarmReport createdAlarm2 = rulesDao.createAlarmReport(alarm2);
        
        String response = rulesService.reprocessAlarm(Arrays.asList(createdAlarm1.getGuid(), createdAlarm2.getGuid()), "Test user");
        assertThat(response, is("OK"));
        
        AlarmReport fetchedAlarm1 = rulesService.getAlarmReportByGuid(createdAlarm1.getGuid());
        assertThat(fetchedAlarm1.getStatus(), is(AlarmStatusType.REPROCESSED.value()));
        AlarmReport fetchedAlarm2 = rulesService.getAlarmReportByGuid(createdAlarm1.getGuid());
        assertThat(fetchedAlarm2.getStatus(), is(AlarmStatusType.REPROCESSED.value()));
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
        action.setAction(ActionType.SEND_TO_FLUX);
        action.setValue("FLUX DNK");
        action.setOrder("0");


        RuleAction ruleAction = new RuleAction();
        ruleAction.setAction(ActionType.SEND_TO_FLUX.value());
        ruleAction.setValue("FLUX DNK");
        ruleAction.setOrder(0);
        ruleAction.setCustomRule(customRule);

        customRule.getRuleActionList().add(ruleAction);

        return customRule;
    }
    
    private Ticket getBasicTicket() {
        Ticket ticket = new Ticket();
        ticket.setAssetGuid(UUID.randomUUID().toString());
        ticket.setStatus(TicketStatusType.OPEN.value());
        ticket.setCreatedDate(new Date());
        ticket.setRuleName("Test Rule");
        ticket.setUpdated(new Date());
        ticket.setUpdatedBy("Test user");
        return ticket;
    }
    
    private AlarmReport getBasicAlarmReport() {
        AlarmReport alarmReport = new AlarmReport();
        alarmReport.setAssetGuid(UUID.randomUUID().toString());
        alarmReport.setStatus(AlarmStatusType.OPEN.value());
        alarmReport.setUpdated(new Date());
        alarmReport.setUpdatedBy("Test user");
        return alarmReport;
    }
    
    private RawMovement getBasicRawMovement() {
        RawMovement rawMovement = new RawMovement();
        rawMovement.setUpdated(new Date());
        rawMovement.setUpdatedBy("Test");
        return rawMovement;
    }
    
}
