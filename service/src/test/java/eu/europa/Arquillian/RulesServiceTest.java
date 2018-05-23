package eu.europa.Arquillian;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.type.CollectionType;
import eu.europa.ec.fisheries.schema.mobileterminal.types.v1.ComChannelType;
import eu.europa.ec.fisheries.schema.rules.alarm.v1.AlarmReportType;
import eu.europa.ec.fisheries.schema.rules.asset.v1.AssetId;
import eu.europa.ec.fisheries.schema.rules.asset.v1.AssetType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.*;
import eu.europa.ec.fisheries.schema.rules.exchange.v1.PluginType;
import eu.europa.ec.fisheries.schema.rules.mobileterminal.v1.MobileTerminalType;
import eu.europa.ec.fisheries.schema.rules.movement.v1.*;
import eu.europa.ec.fisheries.schema.rules.search.v1.*;
import eu.europa.ec.fisheries.schema.rules.source.v1.GetAlarmListByQueryResponse;
import eu.europa.ec.fisheries.schema.rules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.schema.rules.ticket.v1.TicketType;
import eu.europa.ec.fisheries.uvms.rules.exception.InputArgumentException;
import eu.europa.ec.fisheries.uvms.rules.service.RulesService;
import eu.europa.ec.fisheries.uvms.rules.service.exception.RulesServiceException;
import javafx.scene.control.Pagination;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;

import javax.ejb.EJB;
import javax.ejb.EJBTransactionRolledbackException;
import javax.persistence.NoResultException;
import java.nio.file.AccessDeniedException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;


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
        CustomRuleType input = getCompleteNewCustomRule();
        input.setAvailability(AvailabilityType.GLOBAL);

        CustomRuleType output = null;
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

        input.setAvailability((AvailabilityType.PRIVATE));
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
        CustomRuleType input = getCompleteNewCustomRule();
        input.setAvailability(AvailabilityType.GLOBAL);

        //not enough rights
        try {
            rulesService.updateCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch (AccessDeniedException e){
            Assert.assertTrue(true);
        }
        input.setAvailability(AvailabilityType.PRIVATE);

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

        //no such rule to update
        try{
            rulesService.updateCustomRule(input, "test", "test");
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        try{
            rulesService.updateCustomRule(input);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
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

        try{
            rulesService.updateSubscription(input, "testUser"); //non-existant rule guid
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
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
        CustomRuleType newRule = getCompleteNewCustomRule();
        newRule = rulesService.createCustomRule(newRule, "test", "test");
        Assert.assertNotNull(newRule.getGuid());

        input.setRuleGuid(newRule.getGuid());

        input.setOperation(SubscritionOperationType.ADD);
        CustomRuleType output = rulesService.updateSubscription(input, null);
        Assert.assertEquals(2, output.getSubscriptions().size());

        input.setOperation((SubscritionOperationType.REMOVE));
        output = rulesService.updateSubscription(input, null);
        Assert.assertEquals(1, output.getSubscriptions().size());
        Assert.assertEquals("vms_admin_com", output.getSubscriptions().get(0).getOwner());
    }

    //test for getAlarmList among the rest tests

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

        ListPagination lp = new ListPagination();
        input.setPagination(lp);

        try {
            rulesService.getTicketList(null, input);    //missing criteria
            Assert.assertTrue(false);
        }catch (EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
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
    public void updateTicketStatusNegativeTest() throws Exception {     //a test with proper input is among the rest tests
        try{
            rulesService.updateTicketStatus(null);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }

        TicketType input = new TicketType();
        try{
            rulesService.updateTicketStatus(input);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
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
        try{
            rulesService.updateTicketStatusByQuery("test user", input, TicketStatusType.OPEN);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
            Assert.assertTrue(true);
        }
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
        AlarmReportType input = new AlarmReportType();
        try{
            rulesService.updateAlarmStatus(input);
            Assert.assertTrue(false);
        }catch(EJBTransactionRolledbackException e){
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




    private CustomRuleType getCompleteNewCustomRule(){
        CustomRuleType customRule = new CustomRuleType();

        customRule.setName("Flag SWE && area DNK => Send to DNK" + " (" + System.currentTimeMillis() + ")");
        customRule.setAvailability(AvailabilityType.PRIVATE);
        customRule.setUpdatedBy("vms_admin_com");
        customRule.setActive(true);
        customRule.setArchived(false);

        // If flagstate = SWE
        CustomRuleSegmentType flagStateRule = new CustomRuleSegmentType();
        flagStateRule.setStartOperator("(");
        flagStateRule.setCriteria(CriteriaType.ASSET);
        flagStateRule.setSubCriteria(SubCriteriaType.FLAG_STATE);
        flagStateRule.setCondition(ConditionType.EQ);
        flagStateRule.setValue("SWE");
        flagStateRule.setEndOperator(")");
        flagStateRule.setLogicBoolOperator(LogicOperatorType.AND);
        flagStateRule.setOrder("0");
        customRule.getDefinitions().add(flagStateRule);

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
        customRule.getDefinitions().add(areaRule);

        // then send to FLUX DNK
        CustomRuleActionType action = new CustomRuleActionType();
        action.setAction(ActionType.SEND_TO_FLUX);
        action.setValue("FLUX DNK");
        action.setOrder("0");

        customRule.getActions().add(action);

        return customRule;
    }
}
