package eu.europa.ec.fisheries.uvms.rules.service;

import java.nio.file.AccessDeniedException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.ejb.EJB;
import javax.ejb.EJBTransactionRolledbackException;

import eu.europa.ec.fisheries.uvms.rules.entity.*;
import eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException;
import eu.europa.ec.fisheries.uvms.rules.exception.NoEntityFoundException;
import eu.europa.ec.fisheries.uvms.rules.mapper.CustomRuleMapper;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.rules.alarm.v1.AlarmReportType;
import eu.europa.ec.fisheries.schema.rules.asset.v1.AssetId;
import eu.europa.ec.fisheries.schema.rules.asset.v1.AssetType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.ActionType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.ConditionType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.CriteriaType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.CustomRuleActionType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.CustomRuleSegmentType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.LogicOperatorType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SubCriteriaType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SubscriptionType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SubscriptionTypeType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SubscritionOperationType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.UpdateSubscriptionType;
import eu.europa.ec.fisheries.schema.rules.exchange.v1.PluginType;
import eu.europa.ec.fisheries.schema.rules.mobileterminal.v1.MobileTerminalType;
import eu.europa.ec.fisheries.schema.rules.movement.v1.MovementActivityType;
import eu.europa.ec.fisheries.schema.rules.movement.v1.MovementActivityTypeType;
import eu.europa.ec.fisheries.schema.rules.movement.v1.MovementComChannelType;
import eu.europa.ec.fisheries.schema.rules.movement.v1.MovementPoint;
import eu.europa.ec.fisheries.schema.rules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.schema.rules.movement.v1.MovementTypeType;
import eu.europa.ec.fisheries.schema.rules.movement.v1.RawMovementType;
import eu.europa.ec.fisheries.schema.rules.search.v1.ListPagination;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketListCriteria;
import eu.europa.ec.fisheries.schema.rules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.rules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.rules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.uvms.rules.service.RulesService;


@RunWith(Arquillian.class)
public class RulesServiceTest extends TransactionalTests {


    @EJB
    RulesService rulesService;

    @Test
    public void worldsBestTestTest(){
        Assert.assertTrue(true);
    }


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
    public void getCustomRuleByDummyGuidTest() throws Exception{   //a get with proper input exists among the rest tests
        try {
            rulesService.getCustomRuleByGuid("dummyGuid");
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
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
        }catch(InputArgumentException e){
            Assert.assertTrue(true);
        }

        //null as input, funnily enough they die at different places......
        try{
            rulesService.updateCustomRule(null);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
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
    public void deleteCustomRuleWithDummyGuidTest() throws Exception {          //a test with proper exectuion exists among the rest tests
        try {
            rulesService.deleteCustomRule(null, "testUser", "testFeature", "testApp");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try {
            rulesService.deleteCustomRule("dummyGuid", "testUser", "testFeature", "testApp");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }

    @Test
    public void updateSubscriptionNegativeTests() throws Exception{
        try{
            rulesService.updateSubscription(null, "testUser");
            Assert.assertTrue(false);
        }catch (InputArgumentException e){
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
        }catch (InputArgumentException e){
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

    //test for getAlarmList among the rest tests

    @Test
    public void getTicketListNegativeTest() throws Exception {   //a test with proper input is among the rest tests
        try {
            rulesService.getTicketList(null, null);   //missing query
            Assert.assertTrue(false);
        }catch (InputArgumentException e){
            Assert.assertTrue(true);
        }

        TicketQuery input = new TicketQuery();

        try {
            rulesService.getTicketList(null, input);    //missing pagination
            Assert.assertTrue(false);
        }catch (InputArgumentException e){
            Assert.assertTrue(true);
        }
    }

    @Test
    public void getTicketsByMovementsNegativeTest() throws Exception{ //a test with proper input is among the rest tests
        try{
            rulesService.getTicketsByMovements(null);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
            Assert.assertTrue(true);
        }
        List<String> input = new ArrayList<String>();

        try{
            rulesService.getTicketsByMovements(input);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
            Assert.assertTrue(true);
        }

    }
    @Test
    public void countTicketsByMovementNegativeTest() throws Exception { //a test with proper input is among the rest tests
        try{
            rulesService.getTicketsByMovements(null);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
            Assert.assertTrue(true);
        }

        List<String> input = new ArrayList<String>();

        try{
            rulesService.getTicketsByMovements(input);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
            Assert.assertTrue(true);
        }
    }

    @Test
    public void updateTicketStatusNegativeTest() throws Exception {     //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketStatus(null);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
            Assert.assertTrue(true);
        }

        Ticket input = new Ticket();
        try{
            rulesService.updateTicketStatus(input);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
            Assert.assertTrue(true);
        }
    }

    @Test
    public void updateTicketStatusByQueryNegativeTest() throws Exception{   //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketStatusByQuery(null, null, null);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateTicketStatusByQuery("test user", null, null);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateTicketStatusByQuery("test user", null, TicketStatusType.OPEN);
            Assert.assertTrue(false);
        }catch(InputArgumentException e){
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
    public void updateTicketCountNegativeTest() throws Exception {     //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketCount(null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        TicketType input = new TicketType();
        try{
            rulesService.updateTicketCount(input);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
    }

    //getAlarmReportByGuid and getTicketByGuid have tests among the rest tests


    @Test
    public void setMovementReportReceivedTest() throws Exception {
        RawMovementType input = new RawMovementType();
        AssetId assetId = new AssetId();
        assetId.setAssetType(AssetType.AIR);
        input.setAssetId(assetId);

        input.setPositionTime(new Date());
        MovementPoint movementPoint = new MovementPoint();
        movementPoint.setLatitude(0.0D);
        movementPoint.setLongitude(0.0D);
        input.setPosition(movementPoint);

        input.setAssetName("test boat");
        input.setComChannelType(MovementComChannelType.FLUX);
        input.setFlagState("SWE");

        MobileTerminalType mobileTerminalType = new MobileTerminalType();
        mobileTerminalType.setConnectId("test connect id");
        mobileTerminalType.setGuid("test MTT guid");
        input.setMobileTerminal(mobileTerminalType);

        input.setMovementType(MovementTypeType.POS);
        input.setReportedCourse(0.1D);
        input.setReportedSpeed(0.2D);
        input.setSource(MovementSourceType.NAF);

        MovementActivityType movementActivityType = new MovementActivityType();
        movementActivityType.setCallback("callback");
        movementActivityType.setMessageId("test message ID");
        movementActivityType.setMessageType(MovementActivityTypeType.CAT);
        input.setActivity(movementActivityType);
        input.setConnectId("rmt connectID");
        input.setDateRecieved(new Date());
        input.setExternalMarking("marking");
        input.setPluginName("plugin name");
        input.setPluginType(PluginType.FLUX.value());
        input.setStatus("bored");
        input.setAckResponseMessageID("ack message");
        input.setInternalReferenceNumber("42");
        input.setTripNumber(42D);



        try{
            rulesService.setMovementReportReceived(null, null, null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        rulesService.setMovementReportReceived(input, null, null);  //right now this only checks that there are no exceptions thrown


    }




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
}
