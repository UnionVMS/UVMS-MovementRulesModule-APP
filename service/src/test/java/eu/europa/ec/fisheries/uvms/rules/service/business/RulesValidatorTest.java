package eu.europa.ec.fisheries.uvms.rules.service.business;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import javax.inject.Inject;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import eu.europa.ec.fisheries.schema.exchange.plugin.types.v1.PluginType;
import eu.europa.ec.fisheries.schema.rules.alarm.v1.AlarmReportType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.ConditionType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.CriteriaType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.LogicOperatorType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SubCriteriaType;
import eu.europa.ec.fisheries.schema.rules.module.v1.GetAlarmListByQueryResponse;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmListCriteria;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmQuery;
import eu.europa.ec.fisheries.schema.rules.search.v1.AlarmSearchKey;
import eu.europa.ec.fisheries.uvms.rules.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.rules.entity.RuleSegment;
import eu.europa.ec.fisheries.uvms.rules.service.RulesService;
import eu.europa.ec.fisheries.uvms.rules.service.RulesTestHelper;
import eu.europa.ec.fisheries.uvms.rules.service.TransactionalTests;
import eu.europa.ec.fisheries.uvms.rules.service.ValidationService;

@RunWith(Arquillian.class)
public class RulesValidatorTest extends TransactionalTests {

    @Inject
    RulesValidator rulesValidator;
    
    @Inject
    ValidationService validationService;
    
    @Inject
    RulesService rulesService;

    /*
     * Sanity Rules
     */
    
    @Test
    public void triggerLatitudeMustExistRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setLatitude(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("Lat missing", timestamp);
    }
    
    @Test
    public void triggerLongitudeMustExistRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setLongitude(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("Long missing", timestamp);
    }
    
    @Test
    public void triggerTransponderNotFoundRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setPluginType(PluginType.SATELLITE_RECEIVER.value());
        fact.setMobileTerminalConnectId(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("Transponder not found", timestamp);
    }
    
    @Test
    public void triggerAssetNotFoundRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setAssetGuid(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("Asset not found", timestamp);
    }
    
    @Test
    public void triggerMemberNoMissingRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setPluginType(PluginType.SATELLITE_RECEIVER.value());
        fact.setMobileTerminalType("INMARSAT_C");
        fact.setMobileTerminalMemberNumber(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("Mem No. missing", timestamp);
    }
    
    @Test
    public void triggerDNIDMissingRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setPluginType(PluginType.SATELLITE_RECEIVER.value());
        fact.setMobileTerminalType("INMARSAT_C");
        fact.setMobileTerminalDnid(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("DNID missing", timestamp);
    }
    
    @Test
    public void triggerSerialNoMissingRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setPluginType(PluginType.SATELLITE_RECEIVER.value());
        fact.setMobileTerminalType("IRIDIUM");
        fact.setMobileTerminalMemberNumber(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("Serial No. missing", timestamp);
    }
    
    @Test
    public void triggerComChannelTypeMissingRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setComChannelType(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("ComChannel Type missing", timestamp);
    }
    
    @Ignore // Enable this test when liquibase update is released
    @Test
    public void triggerCfrAndIrcsMissingRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setPluginType(PluginType.FLUX.value());
        fact.setIrcs(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("CFR and IRCS missing", timestamp);
    }
    
    @Test
    public void triggerBothCfrAndIrcsMissingRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setPluginType(PluginType.FLUX.value());
        fact.setIrcs(null);
        fact.setCfr(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("CFR and IRCS missing", timestamp);
    }
    
    @Test
    public void triggerPluginTypeMissingRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setPluginType(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("Plugin Type missing", timestamp);
    }

    @Test
    public void triggerFutureDateRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        Calendar calendar = Calendar.getInstance();
        calendar.add(Calendar.HOUR, 1);
        fact.setPositionTime(calendar.getTime());
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("Time in the future", timestamp);
    }
    
    @Test
    public void triggerTimeMissingRule() throws Exception {
        Date timestamp = getTimestamp();
        long ticketsBefore = validationService.getNumberOfOpenAlarmReports();
        
        RawMovementFact fact = RulesTestHelper.createBasicRawMovementFact();
        fact.setPositionTime(null);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenAlarmReports();
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertSanityRuleWasTriggered("Time missing", timestamp);
    }
    
    private void assertSanityRuleWasTriggered(String ruleName, Date fromDate) throws Exception {
        AlarmQuery query = RulesTestHelper.getBasicAlarmQuery();
        AlarmListCriteria ruleNameCriteria = new AlarmListCriteria();
        ruleNameCriteria.setKey(AlarmSearchKey.RULE_NAME);
        ruleNameCriteria.setValue(ruleName);
        query.getAlarmSearchCriteria().add(ruleNameCriteria);
        AlarmListCriteria dateCriteria = new AlarmListCriteria();
        dateCriteria.setKey(AlarmSearchKey.FROM_DATE);
        dateCriteria.setValue(RulesUtil.dateToString(fromDate));
        query.getAlarmSearchCriteria().add(dateCriteria);
        GetAlarmListByQueryResponse alarmList = rulesService.getAlarmList(query);
        List<AlarmReportType> alarms = alarmList.getAlarms();
        assertThat(alarms.size(), is(1));
    }
    
    /*
     * Custom Rules
     */
    
    @Test
    public void evaluateMovementFactTriggerFlagStateRuleTest() throws Exception {
        Date timestamp = getTimestamp();
        String flagstate = "SWE";

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.ASSET.value());
        segment.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(flagstate);
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setEndOperator("");
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementFact fact = RulesTestHelper.createBasicMovementFact();
        fact.setFlagState(flagstate);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), timestamp);
    }
    
    @Test
    public void evaluateMovementFactDontTriggerFlagStateRuleTest() throws Exception {
        Date timestamp = getTimestamp();
        String flagstate = "SWE";

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setCriteria(CriteriaType.ASSET.value());
        segment.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(flagstate);
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementFact fact = RulesTestHelper.createBasicMovementFact();
        fact.setFlagState("TEST");
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore));
    }
    
    @Test
    public void evaluateMovementFactTriggerPositionTimeRuleTest() throws Exception {
        Date positionTime = getTimestamp();

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment segment = new RuleSegment();
        segment.setStartOperator("");
        segment.setCriteria(CriteriaType.POSITION.value());
        segment.setSubCriteria(SubCriteriaType.POSITION_REPORT_TIME.value());
        segment.setCondition(ConditionType.EQ.value());
        segment.setValue(RulesUtil.dateToString(positionTime));
        segment.setEndOperator("");
        segment.setLogicOperator(LogicOperatorType.NONE.value());
        segment.setOrder(0);
        segment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(segment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementFact fact = RulesTestHelper.createBasicMovementFact();
        fact.setPositionTime(positionTime);
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), positionTime);
    }
    
    @Test
    public void evaluateMovementFactTriggerFlagStateAndAreaRuleTest() throws Exception {
        Date timestamp = getTimestamp();
        String flagstate = "SWE";
        String area = "SWE";

        CustomRule customRule = RulesTestHelper.createBasicCustomRule();
        RuleSegment flagstateSegment = new RuleSegment();
        flagstateSegment.setStartOperator("");
        flagstateSegment.setCriteria(CriteriaType.ASSET.value());
        flagstateSegment.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        flagstateSegment.setCondition(ConditionType.EQ.value());
        flagstateSegment.setValue(flagstate);
        flagstateSegment.setEndOperator("");
        flagstateSegment.setLogicOperator(LogicOperatorType.AND.value());
        flagstateSegment.setOrder(0);
        flagstateSegment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(flagstateSegment);
        RuleSegment areaSegment = new RuleSegment();
        areaSegment.setStartOperator("");
        areaSegment.setCriteria(CriteriaType.AREA.value());
        areaSegment.setSubCriteria(SubCriteriaType.AREA_CODE.value());
        areaSegment.setCondition(ConditionType.EQ.value());
        areaSegment.setValue(area);
        areaSegment.setEndOperator("");
        areaSegment.setLogicOperator(LogicOperatorType.NONE.value());
        areaSegment.setOrder(1);
        areaSegment.setCustomRule(customRule);
        customRule.getRuleSegmentList().add(areaSegment);
        CustomRule createdCustomRule = rulesService.createCustomRule(customRule, "", "");
        
        long ticketsBefore = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        
        MovementFact fact = RulesTestHelper.createBasicMovementFact();
        fact.setFlagState(flagstate);
        fact.setAreaCodes(Arrays.asList(area));
        rulesValidator.evaluate(fact);
        
        long ticketsAfter = validationService.getNumberOfOpenTickets(customRule.getUpdatedBy());
        assertThat(ticketsAfter, is(ticketsBefore + 1));
        
        assertCustomRuleWasTriggered(createdCustomRule.getGuid(), timestamp);
    }
    
    private void assertCustomRuleWasTriggered(String ruleGuid, Date fromDate) throws Exception {
        CustomRule customRule = rulesService.getCustomRuleByGuid(ruleGuid);
        assertThat(customRule.getTriggered(), is(notNullValue()));
        assertTrue(customRule.getTriggered().after(fromDate)
                || customRule.getTriggered().equals(fromDate));
    }
    
    private Date getTimestamp() {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MILLISECOND, 0);
        return calendar.getTime();
    }
}
