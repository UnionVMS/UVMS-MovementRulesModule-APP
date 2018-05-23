/*
﻿Developed with the contribution of the European Commission - Directorate General for Maritime Affairs and Fisheries
© European Union, 2015-2016.

This file is part of the Integrated Fisheries Data Management (IFDM) Suite. The IFDM Suite is free software: you can
redistribute it and/or modify it under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or any later version. The IFDM Suite is distributed in
the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a
copy of the GNU General Public License along with the IFDM Suite. If not, see <http://www.gnu.org/licenses/>.
 */
package eu.europa.fisheries.uvms.rules.service.business;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.ActionType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.AvailabilityType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.ConditionType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.CriteriaType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.LogicOperatorType;
import eu.europa.ec.fisheries.schema.rules.customrule.v1.SubCriteriaType;
import eu.europa.ec.fisheries.uvms.rules.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.rules.entity.Interval;
import eu.europa.ec.fisheries.uvms.rules.entity.RuleAction;
import eu.europa.ec.fisheries.uvms.rules.entity.RuleSegment;
import eu.europa.ec.fisheries.uvms.rules.service.business.CustomRuleDto;
import eu.europa.ec.fisheries.uvms.rules.service.business.RulesUtil;
import eu.europa.ec.fisheries.uvms.rules.service.mapper.CustomRuleParser;

public class CustomRuleParserTest {
  
    @Test
    public void testResourceDrl() throws Exception {
        List<CustomRule> rawRules = new ArrayList<CustomRule>();

        CustomRule customRule = new CustomRule();
        customRule.setName("DummyName");
        customRule.setAvailability(AvailabilityType.PRIVATE.value());

        // First part of rule
        RuleSegment segment1 = new RuleSegment();
        segment1.setStartOperator("(");
        segment1.setCriteria(CriteriaType.ASSET.value());
        segment1.setSubCriteria(SubCriteriaType.ASSET_CFR.value());
        segment1.setCondition(ConditionType.EQ.value());
        segment1.setValue("\"SWE111111\"");  // Test that quotation is removed
        segment1.setEndOperator("");
        segment1.setLogicOperator(LogicOperatorType.OR.value());
        segment1.setOrder(0);
        customRule.getRuleSegmentList().add(segment1);

        // Second part of rule
        RuleSegment segment2 = new RuleSegment();
        segment2.setStartOperator("");
        segment2.setCriteria(CriteriaType.ASSET.value());
        segment2.setSubCriteria(SubCriteriaType.ASSET_CFR.value());
        segment2.setCondition(ConditionType.EQ.value());
        segment2.setValue("SWE222222");
        segment2.setEndOperator(")");
        segment2.setLogicOperator(LogicOperatorType.AND.value());
        segment2.setOrder(1);
        customRule.getRuleSegmentList().add(segment2);

        // Third part of rule
        RuleSegment segment3 = new RuleSegment();
        segment3.setStartOperator("");
        segment3.setCriteria(CriteriaType.MOBILE_TERMINAL.value());
        segment3.setSubCriteria(SubCriteriaType.MT_MEMBER_ID.value());
        segment3.setCondition(ConditionType.EQ.value());
        segment3.setValue("ABC99");
        segment3.setEndOperator("");
        segment3.setLogicOperator(LogicOperatorType.NONE.value());
        segment3.setOrder(2);
        customRule.getRuleSegmentList().add(segment3);

        // First interval
        Interval interval1 = new Interval();
        String interval1start = "2000-10-01 02:00:00 +0200";
        interval1.setStart(RulesUtil.stringToDate(interval1start));
        String interval1end = "2000-10-30 01:00:00 +0100";
        interval1.setEnd(RulesUtil.stringToDate(interval1end));
        customRule.getIntervals().add(interval1);

        // First interval
        Interval interval2 = new Interval();
        String interval2start = "2015-01-01 01:00:00 +0100";
        interval2.setStart(RulesUtil.stringToDate(interval2start));
        String interval2end = "2016-12-31 01:00:00 +0100";
        interval2.setEnd(RulesUtil.stringToDate(interval2end));
        customRule.getIntervals().add(interval2);

        // First action
        RuleAction action1 = new RuleAction();
        action1.setAction(ActionType.EMAIL.value());
        action1.setValue("user@company.se");
        customRule.getRuleActionList().add(action1);

        // Second action
        RuleAction action2 = new RuleAction();
        action2.setAction(ActionType.SEND_TO_FLUX.value());
        action2.setValue("DNK");
        customRule.getRuleActionList().add(action2);

        // Third action
        RuleAction action3 = new RuleAction();
        action3.setAction(ActionType.SEND_TO_NAF.value());
        action3.setValue("DNK");
        customRule.getRuleActionList().add(action3);

        rawRules.add(customRule);

        String expectedRule =
                "(cfr == \"SWE111111\" || cfr == \"SWE222222\") && mobileTerminalMemberNumber == \"ABC99\" && (" + RulesUtil.stringToDate(interval1start) + " <= positionTime && positionTime <= " + RulesUtil.stringToDate(interval1end) + " || " + RulesUtil.stringToDate(interval2start) + " <= positionTime && positionTime <= " + RulesUtil.stringToDate(interval2end) + ")";

        List<CustomRuleDto> rules = CustomRuleParser.parseRules(rawRules);
        assertEquals(expectedRule, rules.get(0)
                .getExpression());
        assertEquals("EMAIL,user@company.se;SEND_TO_FLUX,DNK;SEND_TO_NAF,DNK;", rules.get(0).getAction());

    }

    @Test
    public void testAllCriterias() throws Exception {
        List<CustomRule> rawRules = new ArrayList<CustomRule>();

        CustomRule customRule = new CustomRule();
        customRule.setName("DummyName");
        customRule.setAvailability(AvailabilityType.PRIVATE.value());

        // ACTIVITY_CALLBACK
        RuleSegment segment0 = new RuleSegment();
        segment0.setStartOperator("");
        segment0.setCriteria(CriteriaType.ACTIVITY.value());
        segment0.setSubCriteria(SubCriteriaType.ACTIVITY_CALLBACK.value());
        segment0.setCondition(ConditionType.EQ.value());
        segment0.setValue("ACTIVITY_CALLBACK");
        segment0.setEndOperator("");
        segment0.setLogicOperator(LogicOperatorType.OR.value());
        segment0.setOrder(0);
        customRule.getRuleSegmentList().add(segment0);

        // ACTIVITY_MESSAGE_ID
        RuleSegment segment1 = new RuleSegment();
        segment1.setStartOperator("");
        segment1.setCriteria(CriteriaType.ACTIVITY.value());
        segment1.setSubCriteria(SubCriteriaType.ACTIVITY_MESSAGE_ID.value());
        segment1.setCondition(ConditionType.EQ.value());
        segment1.setValue("ACTIVITY_MESSAGE_ID");
        segment1.setEndOperator("");
        segment1.setLogicOperator(LogicOperatorType.OR.value());
        segment1.setOrder(1);
        customRule.getRuleSegmentList().add(segment1);

        // ACTIVITY_MESSAGE_TYPE
        RuleSegment segment2 = new RuleSegment();
        segment2.setStartOperator("");
        segment2.setCriteria(CriteriaType.ACTIVITY.value());
        segment2.setSubCriteria(SubCriteriaType.ACTIVITY_MESSAGE_TYPE.value());
        segment2.setCondition(ConditionType.EQ.value());
        segment2.setValue("ACTIVITY_MESSAGE_TYPE");
        segment2.setEndOperator("");
        segment2.setLogicOperator(LogicOperatorType.OR.value());
        segment2.setOrder(2);
        customRule.getRuleSegmentList().add(segment2);

        // AREA_CODE
        RuleSegment segment3 = new RuleSegment();
        segment3.setStartOperator("");
        segment3.setCriteria(CriteriaType.AREA.value());
        segment3.setSubCriteria(SubCriteriaType.AREA_CODE.value());
        segment3.setCondition(ConditionType.EQ.value());
        segment3.setValue("DNK");
        segment3.setEndOperator("");
        segment3.setLogicOperator(LogicOperatorType.OR.value());
        segment3.setOrder(3);
        customRule.getRuleSegmentList().add(segment3);

        // AREA_TYPE
        RuleSegment segment4 = new RuleSegment();
        segment4.setStartOperator("");
        segment4.setCriteria(CriteriaType.AREA.value());
        segment4.setSubCriteria(SubCriteriaType.AREA_TYPE.value());
        segment4.setCondition(ConditionType.NE.value());
        segment4.setValue("EEZ");
        segment4.setEndOperator("");
        segment4.setLogicOperator(LogicOperatorType.OR.value());
        segment4.setOrder(4);
        customRule.getRuleSegmentList().add(segment4);

        // ASSET_ID_GEAR_TYPE
        RuleSegment segment5 = new RuleSegment();
        segment5.setStartOperator("");
        segment5.setCriteria(CriteriaType.ASSET.value());
        segment5.setSubCriteria(SubCriteriaType.ASSET_ID_GEAR_TYPE.value());
        segment5.setCondition(ConditionType.EQ.value());
        segment5.setValue("ASSET_ID_GEAR_TYPE");
        segment5.setEndOperator("");
        segment5.setLogicOperator(LogicOperatorType.OR.value());
        segment5.setOrder(5);
        customRule.getRuleSegmentList().add(segment5);

        // EXTERNAL_MARKING
        RuleSegment segment6 = new RuleSegment();
        segment6.setStartOperator("");
        segment6.setCriteria(CriteriaType.ASSET.value());
        segment6.setSubCriteria(SubCriteriaType.EXTERNAL_MARKING.value());
        segment6.setCondition(ConditionType.EQ.value());
        segment6.setValue("GE-49");
        segment6.setEndOperator("");
        segment6.setLogicOperator(LogicOperatorType.OR.value());
        segment6.setOrder(6);
        customRule.getRuleSegmentList().add(segment6);

        // FLAG_STATE
        RuleSegment segment7 = new RuleSegment();
        segment7.setStartOperator("");
        segment7.setCriteria(CriteriaType.ASSET.value());
        segment7.setSubCriteria(SubCriteriaType.FLAG_STATE.value());
        segment7.setCondition(ConditionType.EQ.value());
        segment7.setValue("SWE");
        segment7.setEndOperator("");
        segment7.setLogicOperator(LogicOperatorType.OR.value());
        segment7.setOrder(7);
        customRule.getRuleSegmentList().add(segment7);

        // ASSET_CFR
        RuleSegment segment8 = new RuleSegment();
        segment8.setStartOperator("");
        segment8.setCriteria(CriteriaType.ASSET.value());
        segment8.setSubCriteria(SubCriteriaType.ASSET_CFR.value());
        segment8.setCondition(ConditionType.EQ.value());
        segment8.setValue("GBR000A11447");
        segment8.setEndOperator("");
        segment8.setLogicOperator(LogicOperatorType.OR.value());
        segment8.setOrder(8);
        customRule.getRuleSegmentList().add(segment8);

        // ASSET_IRCS
        RuleSegment segment9 = new RuleSegment();
        segment9.setStartOperator("");
        segment9.setCriteria(CriteriaType.ASSET.value());
        segment9.setSubCriteria(SubCriteriaType.ASSET_IRCS.value());
        segment9.setCondition(ConditionType.EQ.value());
        segment9.setValue("SECT");
        segment9.setEndOperator("");
        segment9.setLogicOperator(LogicOperatorType.OR.value());
        segment9.setOrder(9);
        customRule.getRuleSegmentList().add(segment9);

        // ASSET_NAME
        RuleSegment segment10 = new RuleSegment();
        segment10.setStartOperator("");
        segment10.setCriteria(CriteriaType.ASSET.value());
        segment10.setSubCriteria(SubCriteriaType.ASSET_NAME.value());
        segment10.setCondition(ConditionType.EQ.value());
        segment10.setValue("SETTE MARI");
        segment10.setEndOperator("");
        segment10.setLogicOperator(LogicOperatorType.OR.value());
        segment10.setOrder(10);
        customRule.getRuleSegmentList().add(segment10);

        // COMCHANNEL_TYPE
        RuleSegment segment11 = new RuleSegment();
        segment11.setStartOperator("");
        segment11.setCriteria(CriteriaType.MOBILE_TERMINAL.value());
        segment11.setSubCriteria(SubCriteriaType.COMCHANNEL_TYPE.value());
        segment11.setCondition(ConditionType.EQ.value());
        segment11.setValue("VMS");
        segment11.setEndOperator("");
        segment11.setLogicOperator(LogicOperatorType.OR.value());
        segment11.setOrder(11);
        customRule.getRuleSegmentList().add(segment11);

        // MT_TYPE
        RuleSegment segment12 = new RuleSegment();
        segment12.setStartOperator("");
        segment12.setCriteria(CriteriaType.MOBILE_TERMINAL.value());
        segment12.setSubCriteria(SubCriteriaType.MT_TYPE.value());
        segment12.setCondition(ConditionType.EQ.value());
        segment12.setValue("MT_TYPE");
        segment12.setEndOperator("");
        segment12.setLogicOperator(LogicOperatorType.OR.value());
        segment12.setOrder(12);
        customRule.getRuleSegmentList().add(segment12);

        // MT_DNID
        RuleSegment segment13 = new RuleSegment();
        segment13.setStartOperator("");
        segment13.setCriteria(CriteriaType.MOBILE_TERMINAL.value());
        segment13.setSubCriteria(SubCriteriaType.MT_DNID.value());
        segment13.setCondition(ConditionType.EQ.value());
        segment13.setValue("MT_DNID");
        segment13.setEndOperator("");
        segment13.setLogicOperator(LogicOperatorType.OR.value());
        segment13.setOrder(13);
        customRule.getRuleSegmentList().add(segment13);

        // MT_MEMBER_ID
        RuleSegment segment14 = new RuleSegment();
        segment14.setStartOperator("");
        segment14.setCriteria(CriteriaType.MOBILE_TERMINAL.value());
        segment14.setSubCriteria(SubCriteriaType.MT_MEMBER_ID.value());
        segment14.setCondition(ConditionType.EQ.value());
        segment14.setValue("MT_MEMBER_ID");
        segment14.setEndOperator("");
        segment14.setLogicOperator(LogicOperatorType.OR.value());
        segment14.setOrder(14);
        customRule.getRuleSegmentList().add(segment14);

        // MT_SERIAL_NO
        RuleSegment segment15 = new RuleSegment();
        segment15.setStartOperator("");
        segment15.setCriteria(CriteriaType.MOBILE_TERMINAL.value());
        segment15.setSubCriteria(SubCriteriaType.MT_SERIAL_NO.value());
        segment15.setCondition(ConditionType.EQ.value());
        segment15.setValue("MT_SERIAL_NO");
        segment15.setEndOperator("");
        segment15.setLogicOperator(LogicOperatorType.OR.value());
        segment15.setOrder(15);
        customRule.getRuleSegmentList().add(segment15);

        // ALTITUDE
        RuleSegment segment16 = new RuleSegment();
        segment16.setStartOperator("");
        segment16.setCriteria(CriteriaType.POSITION.value());
        segment16.setSubCriteria(SubCriteriaType.ALTITUDE.value());
        segment16.setCondition(ConditionType.EQ.value());
        segment16.setValue("0");
        segment16.setEndOperator("");
        segment16.setLogicOperator(LogicOperatorType.OR.value());
        segment16.setOrder(16);
        customRule.getRuleSegmentList().add(segment16);

        // LATITUDE
        RuleSegment segment17 = new RuleSegment();
        segment17.setStartOperator("");
        segment17.setCriteria(CriteriaType.POSITION.value());
        segment17.setSubCriteria(SubCriteriaType.LATITUDE.value());
        segment17.setCondition(ConditionType.EQ.value());
        segment17.setValue("57");
        segment17.setEndOperator("");
        segment17.setLogicOperator(LogicOperatorType.OR.value());
        segment17.setOrder(17);
        customRule.getRuleSegmentList().add(segment17);

        // LONGITUDE
        RuleSegment segment18 = new RuleSegment();
        segment18.setStartOperator("");
        segment18.setCriteria(CriteriaType.POSITION.value());
        segment18.setSubCriteria(SubCriteriaType.LONGITUDE.value());
        segment18.setCondition(ConditionType.EQ.value());
        segment18.setValue("11");
        segment18.setEndOperator("");
        segment18.setLogicOperator(LogicOperatorType.OR.value());
        segment18.setOrder(18);
        customRule.getRuleSegmentList().add(segment18);

        // CALCULATED_COURSE
        RuleSegment segment19 = new RuleSegment();
        segment19.setStartOperator("");
        segment19.setCriteria(CriteriaType.POSITION.value());
        segment19.setSubCriteria(SubCriteriaType.CALCULATED_COURSE.value());
        segment19.setCondition(ConditionType.LE.value());
        segment19.setValue("46");
        segment19.setEndOperator("");
        segment19.setLogicOperator(LogicOperatorType.OR.value());
        segment19.setOrder(19);
        customRule.getRuleSegmentList().add(segment19);

        // CALCULATED_SPEED
        RuleSegment segment20 = new RuleSegment();
        segment20.setStartOperator("");
        segment20.setCriteria(CriteriaType.POSITION.value());
        segment20.setSubCriteria(SubCriteriaType.CALCULATED_SPEED.value());
        segment20.setCondition(ConditionType.GE.value());
        segment20.setValue("10.1");
        segment20.setEndOperator("");
        segment20.setLogicOperator(LogicOperatorType.OR.value());
        segment20.setOrder(20);
        customRule.getRuleSegmentList().add(segment20);

        // MOVEMENT_TYPE
        RuleSegment segment21 = new RuleSegment();
        segment21.setStartOperator("");
        segment21.setCriteria(CriteriaType.POSITION.value());
        segment21.setSubCriteria(SubCriteriaType.MOVEMENT_TYPE.value());
        segment21.setCondition(ConditionType.EQ.value());
        segment21.setValue("POS");
        segment21.setEndOperator("");
        segment21.setLogicOperator(LogicOperatorType.OR.value());
        segment21.setOrder(21);
        customRule.getRuleSegmentList().add(segment21);

        // POSITION_REPORT_TIME
        RuleSegment segment22 = new RuleSegment();
        segment22.setStartOperator("");
        segment22.setCriteria(CriteriaType.POSITION.value());
        segment22.setSubCriteria(SubCriteriaType.POSITION_REPORT_TIME.value());
        segment22.setCondition(ConditionType.EQ.value());
        segment22.setValue("2000-10-30 01:00:00 +0100");
        segment22.setEndOperator("");
        segment22.setLogicOperator(LogicOperatorType.OR.value());
        segment22.setOrder(22);
        customRule.getRuleSegmentList().add(segment22);

        // REPORTED_COURSE
        RuleSegment segment23 = new RuleSegment();
        segment23.setStartOperator("");
        segment23.setCriteria(CriteriaType.POSITION.value());
        segment23.setSubCriteria(SubCriteriaType.REPORTED_COURSE.value());
        segment23.setCondition(ConditionType.LT.value());
        segment23.setValue("45");
        segment23.setEndOperator("");
        segment23.setLogicOperator(LogicOperatorType.OR.value());
        segment23.setOrder(23);
        customRule.getRuleSegmentList().add(segment23);

        // REPORTED_SPEED
        RuleSegment segment24 = new RuleSegment();
        segment24.setStartOperator("");
        segment24.setCriteria(CriteriaType.POSITION.value());
        segment24.setSubCriteria(SubCriteriaType.REPORTED_SPEED.value());
        segment24.setCondition(ConditionType.GT.value());
        segment24.setValue("10");
        segment24.setEndOperator("");
        segment24.setLogicOperator(LogicOperatorType.OR.value());
        segment24.setOrder(24);
        customRule.getRuleSegmentList().add(segment24);

        // SEGMENT_TYPE
        RuleSegment segment25 = new RuleSegment();
        segment25.setStartOperator("");
        segment25.setCriteria(CriteriaType.POSITION.value());
        segment25.setSubCriteria(SubCriteriaType.SEGMENT_TYPE.value());
        segment25.setCondition(ConditionType.NE.value());
        segment25.setValue("GAP");
        segment25.setEndOperator("");
        segment25.setLogicOperator(LogicOperatorType.OR.value());
        segment25.setOrder(25);
        customRule.getRuleSegmentList().add(segment25);

        // SOURCE
        RuleSegment segment26 = new RuleSegment();
        segment26.setStartOperator("");
        segment26.setCriteria(CriteriaType.POSITION.value());
        segment26.setSubCriteria(SubCriteriaType.SOURCE.value());
        segment26.setCondition(ConditionType.EQ.value());
        segment26.setValue("INMARSAT_C");
        segment26.setEndOperator("");
        segment26.setLogicOperator(LogicOperatorType.OR.value());
        segment26.setOrder(26);
        customRule.getRuleSegmentList().add(segment26);

        // STATUS_CODE
        RuleSegment segment27 = new RuleSegment();
        segment27.setStartOperator("");
        segment27.setCriteria(CriteriaType.POSITION.value());
        segment27.setSubCriteria(SubCriteriaType.STATUS_CODE.value());
        segment27.setCondition(ConditionType.EQ.value());
        segment27.setValue("010");
        segment27.setEndOperator("");
        segment27.setLogicOperator(LogicOperatorType.OR.value());
        segment27.setOrder(27);
        customRule.getRuleSegmentList().add(segment27);

        // CLOSEST_COUNTRY_CODE
        RuleSegment segment28 = new RuleSegment();
        segment28.setStartOperator("");
        segment28.setCriteria(CriteriaType.POSITION.value());
        segment28.setSubCriteria(SubCriteriaType.CLOSEST_COUNTRY_CODE.value());
        segment28.setCondition(ConditionType.EQ.value());
        segment28.setValue("DNK");
        segment28.setEndOperator("");
        segment28.setLogicOperator(LogicOperatorType.OR.value());
        segment28.setOrder(28);
        customRule.getRuleSegmentList().add(segment28);

        // CLOSEST_PORT_CODE
        RuleSegment segment29 = new RuleSegment();
        segment29.setStartOperator("");
        segment29.setCriteria(CriteriaType.POSITION.value());
        segment29.setSubCriteria(SubCriteriaType.CLOSEST_PORT_CODE.value());
        segment29.setCondition(ConditionType.EQ.value());
        segment29.setValue("CLOSEST_PORT_CODE");
        segment29.setEndOperator("");
        segment29.setLogicOperator(LogicOperatorType.OR.value());
        segment29.setOrder(29);
        customRule.getRuleSegmentList().add(segment29);

        // ASSET_GROUP
        RuleSegment segment30 = new RuleSegment();
        segment30.setStartOperator("");
        segment30.setCriteria(CriteriaType.ASSET_GROUP.value());
        segment30.setSubCriteria(SubCriteriaType.ASSET_GROUP.value());
        segment30.setCondition(ConditionType.NE.value());
        segment30.setValue("ASSET_GROUP");
        segment30.setEndOperator("");
        segment30.setLogicOperator(LogicOperatorType.OR.value());
        segment30.setOrder(30);
        customRule.getRuleSegmentList().add(segment30);

        // REPORT
        RuleSegment segment31 = new RuleSegment();
        segment31.setStartOperator("");
        segment31.setCriteria(CriteriaType.REPORT.value());
        segment31.setSubCriteria(SubCriteriaType.SUM_POSITION_REPORT.value());
        segment31.setCondition(ConditionType.GT.value());
        segment31.setValue("10");
        segment31.setEndOperator("");
        segment31.setLogicOperator(LogicOperatorType.OR.value());
        segment31.setOrder(31);
        customRule.getRuleSegmentList().add(segment31);

        // REPORT
        RuleSegment segment32 = new RuleSegment();
        segment32.setStartOperator("");
        segment32.setCriteria(CriteriaType.REPORT.value());
        segment32.setSubCriteria(SubCriteriaType.TIME_DIFF_POSITION_REPORT.value());
        segment32.setCondition(ConditionType.LT.value());
        segment32.setValue("60");
        segment32.setEndOperator("");
        segment32.setLogicOperator(LogicOperatorType.OR.value());
        segment32.setOrder(32);
        customRule.getRuleSegmentList().add(segment32);


        // AREA_CODE_ENT
        RuleSegment segment33 = new RuleSegment();
        segment33.setStartOperator("");
        segment33.setCriteria(CriteriaType.AREA.value());
        segment33.setSubCriteria(SubCriteriaType.AREA_CODE_ENT.value());
        segment33.setCondition(ConditionType.EQ.value());
        segment33.setValue("DNK");
        segment33.setEndOperator("");
        segment33.setLogicOperator(LogicOperatorType.OR.value());
        segment33.setOrder(33);
        customRule.getRuleSegmentList().add(segment33);

        // AREA_TYPE_ENT
        RuleSegment segment34 = new RuleSegment();
        segment34.setStartOperator("");
        segment34.setCriteria(CriteriaType.AREA.value());
        segment34.setSubCriteria(SubCriteriaType.AREA_TYPE_ENT.value());
        segment34.setCondition(ConditionType.NE.value());
        segment34.setValue("EEZ");
        segment34.setEndOperator("");
        segment34.setLogicOperator(LogicOperatorType.OR.value());
        segment34.setOrder(34);
        customRule.getRuleSegmentList().add(segment34);

        // AREA_CODE_EXT
        RuleSegment segment35 = new RuleSegment();
        segment35.setStartOperator("");
        segment35.setCriteria(CriteriaType.AREA.value());
        segment35.setSubCriteria(SubCriteriaType.AREA_CODE_EXT.value());
        segment35.setCondition(ConditionType.EQ.value());
        segment35.setValue("SWE");
        segment35.setEndOperator("");
        segment35.setLogicOperator(LogicOperatorType.OR.value());
        segment35.setOrder(35);
        customRule.getRuleSegmentList().add(segment35);

        // AREA_TYPE_EXT
        RuleSegment segment36 = new RuleSegment();
        segment36.setStartOperator("");
        segment36.setCriteria(CriteriaType.AREA.value());
        segment36.setSubCriteria(SubCriteriaType.AREA_TYPE_EXT.value());
        segment36.setCondition(ConditionType.NE.value());
        segment36.setValue("EEZ");
        segment36.setEndOperator("");
        segment36.setLogicOperator(LogicOperatorType.OR.value());
        segment36.setOrder(36);
        customRule.getRuleSegmentList().add(segment36);

        // ASSET_STATUS
        RuleSegment segment37 = new RuleSegment();
        segment37.setStartOperator("");
        segment37.setCriteria(CriteriaType.ASSET.value());
        segment37.setSubCriteria(SubCriteriaType.ASSET_STATUS.value());
        segment37.setCondition(ConditionType.EQ.value());
        segment37.setValue("ACTIVE");
        segment37.setEndOperator("");
        segment37.setLogicOperator(LogicOperatorType.OR.value());
        segment37.setOrder(37);
        customRule.getRuleSegmentList().add(segment37);

        // MT_STATUS
        RuleSegment segment38 = new RuleSegment();
        segment38.setStartOperator("");
        segment38.setCriteria(CriteriaType.MOBILE_TERMINAL.value());
        segment38.setSubCriteria(SubCriteriaType.MT_STATUS.value());
        segment38.setCondition(ConditionType.NE.value());
        segment38.setValue("INACTIVE");
        segment38.setEndOperator("");
        segment38.setLogicOperator(LogicOperatorType.NONE.value());
        segment38.setOrder(38);
        customRule.getRuleSegmentList().add(segment38);



        // VICINITY_OF
//        RuleSegment segment33 = new RuleSegment();
//        segment33.setStartOperator("");
//        segment33.setCriteria(CriteriaType.POSITION);
//        segment33.setSubCriteria(SubCriteriaType.VICINITY_OF);
//        segment33.setCondition(ConditionType.EQ);
//        segment33.setValue("VICINITY_OF");
//        segment33.setEndOperator(")");
//        segment33.setLogicOperator(LogicOperatorType.NONE);
//        segment33.setOrder("33");
//        customRule.getDefinitions().add(segment33);

        // Action
        RuleAction action = new RuleAction();
        action.setAction(ActionType.SEND_TO_FLUX.value());
        action.setValue("DNK");
        action.setOrder(0);
        customRule.getRuleActionList().add(action);

        rawRules.add(customRule);

        StringBuilder sb = new StringBuilder();
        sb.append("activityCallback == \"ACTIVITY_CALLBACK\" || ");
        sb.append("activityMessageId == \"ACTIVITY_MESSAGE_ID\" || ");
        sb.append("activityMessageType == \"ACTIVITY_MESSAGE_TYPE\" || ");
        sb.append("areaCodes.contains(\"DNK\") || ");
        sb.append("!areaTypes.contains(\"EEZ\") || ");
        sb.append("assetIdGearType == \"ASSET_ID_GEAR_TYPE\" || ");
        sb.append("externalMarking == \"GE-49\" || ");
        sb.append("flagState == \"SWE\" || ");
        sb.append("cfr == \"GBR000A11447\" || ");
        sb.append("ircs == \"SECT\" || ");
        sb.append("assetName == \"SETTE MARI\" || ");
        sb.append("comChannelType == \"VMS\" || ");
        sb.append("mobileTerminalType == \"MT_TYPE\" || ");
        sb.append("mobileTerminalDnid == \"MT_DNID\" || ");
        sb.append("mobileTerminalMemberNumber == \"MT_MEMBER_ID\" || ");
        sb.append("mobileTerminalSerialNumber == \"MT_SERIAL_NO\" || ");
        sb.append("altitude == \"0\" || ");
        sb.append("latitude == \"57\" || ");
        sb.append("longitude == \"11\" || ");
        sb.append("calculatedCourse <= \"46\" || ");
        sb.append("calculatedSpeed >= \"10.1\" || ");
        sb.append("movementType == \"POS\" || ");
        sb.append("positionTime == RulesUtil.stringToDate(\"2000-10-30 01:00:00 +0100\") || ");
        sb.append("reportedCourse < \"45\" || ");
        sb.append("reportedSpeed > \"10\" || ");
        sb.append("segmentType != \"GAP\" || ");
        sb.append("source == \"INMARSAT_C\" || ");
        sb.append("statusCode == \"010\" || ");
        sb.append("closestCountryCode == \"DNK\" || ");
        sb.append("closestPortCode == \"CLOSEST_PORT_CODE\" || ");
        sb.append("!assetGroups.contains(\"ASSET_GROUP\") || ");
        sb.append("sumPositionReport > \"10\" || ");
        sb.append("timeDiffPositionReport < \"60\" || ");
        sb.append("entAreaCodes.contains(\"DNK\") || ");
        sb.append("!entAreaTypes.contains(\"EEZ\") || ");
        sb.append("extAreaCodes.contains(\"SWE\") || ");
        sb.append("!extAreaTypes.contains(\"EEZ\") || ");
        sb.append("assetStatus == \"ACTIVE\" || ");
        sb.append("mobileTerminalStatus != \"INACTIVE\"");
//        sb.append("vicinityOf == \"VICINITY_OF\"");

        String expectedRule = sb.toString();

        List<CustomRuleDto> rules = CustomRuleParser.parseRules(rawRules);
        assertEquals(expectedRule, rules.get(0).getExpression());
        assertEquals("SEND_TO_FLUX,DNK;", rules.get(0).getAction());

    }

    @Test
    public void testNullStartAndEndOperator() throws Exception {
        List<CustomRule> rawRules = new ArrayList<CustomRule>();

        CustomRule customRule = new CustomRule();
        customRule.setName("DummyName");
        customRule.setAvailability(AvailabilityType.PRIVATE.value());

        // ACTIVITY_CALLBACK
        RuleSegment segment0 = new RuleSegment();
        segment0.setCriteria(CriteriaType.ACTIVITY.value());
        segment0.setSubCriteria(SubCriteriaType.ACTIVITY_CALLBACK.value());
        segment0.setCondition(ConditionType.EQ.value());
        segment0.setValue("ACTIVITY_CALLBACK");
        segment0.setLogicOperator(LogicOperatorType.OR.value());
        segment0.setOrder(0);
        customRule.getRuleSegmentList().add(segment0);

        // Action
        RuleAction action = new RuleAction();
        action.setAction(ActionType.SEND_TO_FLUX.value());
        action.setValue("DNK");
        action.setOrder(0);
        customRule.getRuleActionList().add(action);

        rawRules.add(customRule);

        StringBuilder sb = new StringBuilder();
        sb.append("activityCallback == \"ACTIVITY_CALLBACK\" || ");

        String expectedRule = sb.toString();

        List<CustomRuleDto> rules = CustomRuleParser.parseRules(rawRules);
        assertEquals(expectedRule, rules.get(0).getExpression());
        assertEquals("SEND_TO_FLUX,DNK;", rules.get(0).getAction());

    }

}