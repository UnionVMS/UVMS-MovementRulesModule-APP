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
package eu.europa.ec.fisheries.uvms.movementrules.rest.service;

import eu.europa.ec.fisheries.schema.movementrules.asset.v1.AssetType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.*;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementActivityTypeType;
import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.ListPagination;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketListCriteria;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketQuery;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.TicketSearchKey;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.UUID;

public class RulesTestHelper {

    public static CustomRule createBasicCustomRule() {
        CustomRule customRule = new CustomRule();
        customRule.setName("Test Rule " + System.currentTimeMillis());
        customRule.setAvailability(AvailabilityType.PRIVATE.value());
        customRule.setUpdatedBy("Test User");
        customRule.setActive(true);
        customRule.setArchived(false);
        return customRule;
    }
    
    public static TicketQuery getBasicTicketQuery() {
        TicketQuery query = new TicketQuery();
        ListPagination pagination = new ListPagination();
        pagination.setPage(1);
        pagination.setListSize(100);
        query.setPagination(pagination);
        return query;
    }

    public static TicketListCriteria getTicketListCriteria(TicketSearchKey key, String value) {
        TicketListCriteria criteria = new TicketListCriteria();
        criteria.setKey(key);
        criteria.setValue(value);
        return criteria;
    }

    public static CustomRuleType getCompleteNewCustomRule(){
        CustomRuleType customRule = new CustomRuleType();

        customRule.setName("Flag SWE && area DNK => Send to DNK" + " (" + System.currentTimeMillis() + ")");
        customRule.setDescription("A description for CustomRule");
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
        action.setAction(ActionType.SEND_REPORT);
        action.setTarget("FLUX");
        action.setValue("FLUX DNK");
        action.setOrder("0");
        customRule.getActions().add(action);

        return customRule;
    }

    public static Ticket getCompleteTicket() {
        Ticket ticket = new Ticket();
        ticket.setUpdated(Instant.now());
        ticket.setCreatedDate(Instant.now());
        ticket.setStatus(TicketStatusType.OPEN);
        ticket.setUpdatedBy("test user");
        ticket.setRuleGuid("tmp rule guid");
        ticket.setAssetGuid("tmp asset guid");
        ticket.setMovementGuid("tmp movement guid");
        ticket.setRuleName("tmp rule name");
        ticket.setTicketCount(1L);
        ticket.setChannelGuid("tmp channel guid");
        ticket.setMobileTerminalGuid(" tmp mobile terminal guid");
        ticket.setRecipient("tmp recipient");

        return ticket;
    }

    public static MovementDetails createBasicMovementDetails() {
        MovementDetails movementDetails = new MovementDetails();
        movementDetails.setLatitude(56d);
        movementDetails.setLongitude(11d);
        movementDetails.setPositionTime(Instant.now());
        movementDetails.setMovementGuid(UUID.randomUUID().toString());
        movementDetails.setAssetGuid(UUID.randomUUID().toString());
        movementDetails.setAssetType(AssetType.VESSEL.value());
        movementDetails.setFlagState("SWE");
        movementDetails.setClosestCountryCode("SWE");
        movementDetails.setClosestPortCode("GBG");
        movementDetails.setAreaCodes(Arrays.asList("DNK"));
        movementDetails.setActivityMessageType(MovementActivityTypeType.COE.value());
        movementDetails.setActivityMessageId("messageId");
        movementDetails.setSource(MovementSourceType.NAF.value());
        movementDetails.setAreaCodes(new ArrayList<>());
        movementDetails.setAreaTypes(new ArrayList<>());
        movementDetails.setEntAreaCodes(new ArrayList<>());
        movementDetails.setExtAreaCodes(new ArrayList<>());
        return movementDetails;
    }
}
