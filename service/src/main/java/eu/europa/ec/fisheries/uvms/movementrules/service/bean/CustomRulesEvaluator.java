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
package eu.europa.ec.fisheries.uvms.movementrules.service.bean;

import eu.europa.ec.fisheries.schema.movementrules.movement.v1.MovementSourceType;
import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.config.exception.ConfigServiceException;
import eu.europa.ec.fisheries.uvms.config.service.ParameterService;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.boundary.SpatialRestClient;
import eu.europa.ec.fisheries.uvms.movementrules.service.business.RulesValidator;
import eu.europa.ec.fisheries.uvms.movementrules.service.config.ParameterKey;
import eu.europa.ec.fisheries.uvms.movementrules.service.constants.ServiceConstants;
import eu.europa.ec.fisheries.uvms.movementrules.service.dao.RulesDao;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.EventTicket;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.PreviousReport;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.Ticket;
import eu.europa.ec.fisheries.uvms.movementrules.service.event.TicketUpdateEvent;
import eu.europa.ec.fisheries.uvms.movementrules.service.message.producer.bean.IncidentProducer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.enterprise.event.Event;
import javax.inject.Inject;
import java.time.Instant;
import java.util.UUID;

@Stateless
public class CustomRulesEvaluator {

    private static final Logger LOG = LoggerFactory.getLogger(CustomRulesEvaluator.class);
    
    @Inject
    private RulesValidator rulesValidator;
    
    @EJB
    private ParameterService parameterService;
    
    @Inject
    private SpatialRestClient spatialClient;
    
    @Inject
    private RulesDao rulesDao;

    @Inject
    @TicketUpdateEvent
    private Event<EventTicket> ticketUpdateEvent;

    @Inject
    private RulesServiceBean rulesServiceBean;

    @Inject
    private IncidentProducer incidentProducer;
    
    public void evaluate(MovementDetails movementDetails) {
        
        Long timeDiffPositionReport = timeDiffAndPersistPreviousReport(movementDetails);

        movementDetails.setTimeDiffPositionReport(timeDiffPositionReport);
        
        spatialClient.populateAreasAndAreaTransitions(movementDetails);
        
        rulesValidator.evaluate(movementDetails);
    }
  
    private Long timeDiffAndPersistPreviousReport(MovementDetails movementDetails) {

        String movementSource = movementDetails.getSource();
        String assetGuid = movementDetails.getAssetGuid();
        String movementId = movementDetails.getMovementGuid();
        String mobTermId = movementDetails.getMobileTerminalGuid();
        String assetFlagState = movementDetails.getFlagState();
        Instant positionTime = movementDetails.getPositionTime();

        // This needs to be done before persisting last report
        Long timeDiffInSeconds = null;
        Long timeDiff = timeDiffFromLastCommunication(assetGuid, positionTime);
        timeDiffInSeconds = timeDiff != null ? timeDiff / 1000 : null;

        // We only persist our own last communications that were not from AIS.
        if (isLocalFlagState(assetFlagState) && !movementSource.equals(MovementSourceType.AIS.value())) {
            if(movementSource.equals(MovementSourceType.MANUAL.value())) {
                checkForOpenAssetNotSendingTicketAndUpdate(assetGuid, movementId);
            } else {
                createIncidentIfAssetIsLongTermParked(movementDetails);
                persistLastCommunication(assetGuid, movementId, mobTermId, positionTime);
                checkForOpenAssetNotSendingTicketAndCloseIt(assetGuid, movementId);
            }
        }

        return timeDiffInSeconds;
    }
    
    
    private Long timeDiffFromLastCommunication(String assetGuid, Instant thisTime) {
        Long timeDiff = null;
        try {
            PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(assetGuid);
            if(entity == null){         //aka not local flag state and not AIS, see line 65
                return null;
            }

            Instant previousTime = entity.getPositionTime();
            timeDiff = thisTime.toEpochMilli() - previousTime.toEpochMilli();
        } catch (Exception e) {
            // If something goes wrong, continue with the other validation
            LOG.error("[ERROR] Error when getting previous report by asset guid {}", e.getMessage());
        }
        return timeDiff;
    }

    private void createIncidentIfAssetIsLongTermParked(MovementDetails movementDetails){
        if(movementDetails.isLongTermParked()){
            Ticket ticket = getAssetSendingDespiteParkedTicket(movementDetails.getAssetGuid());
            if (ticket == null){
                rulesServiceBean.createAssetSendingDespiteLongTermParkedTicket(movementDetails);
                return;
            }
            ticket.setMovementGuid(movementDetails.getMovementGuid());
            ticket.setTicketCount(ticket.getTicketCount() + 1);
            ticketUpdateEvent.fire(new EventTicket(ticket, ServiceConstants.ASSET_SENDING_DESPITE_LONG_TERM_PARKED_CUSTOMRULE));
        }
    }

    private void persistLastCommunication(String assetGuid, String movementId, String mobTermId, Instant positionTime) {
        PreviousReport entity = rulesDao.getPreviousReportByAssetGuid(assetGuid);
        if (entity == null) {
            entity = new PreviousReport();
        }
        entity.setPositionTime(positionTime);
        entity.setAssetGuid(assetGuid);
        if(movementId != null)
            entity.setMovementGuid(UUID.fromString(movementId));
        if (mobTermId != null)
            entity.setMobTermGuid(UUID.fromString(mobTermId));
        entity.setUpdated(Instant.now());
        entity.setUpdatedBy("UVMS");
        rulesDao.updatePreviousReport(entity);
    }
    
    private boolean isLocalFlagState(String flagState) {
        try {
            String localFlagState = parameterService.getStringValue(ParameterKey.LOCAL_FLAGSTATE.getKey());
            return flagState.equalsIgnoreCase(localFlagState);
        } catch (ConfigServiceException e) {
            LOG.error("Could not get local flag state", e);
            return false;
        }
    }


    private void checkForOpenAssetNotSendingTicketAndUpdate(String assetGuid, String movementId) {
        Ticket ticket = getAssetNotSendingTicket(assetGuid);
        if (ticket == null) return;
        ticket.setMovementGuid(movementId);
        CustomRule customRule = rulesServiceBean.getCustomRuleOrAssetNotSendingRule(ticket.getRuleGuid());
        incidentProducer.updatedTicket(new EventTicket(ticket, customRule));
        ticketUpdateEvent.fire(new EventTicket(ticket, customRule));
    }

    private void checkForOpenAssetNotSendingTicketAndCloseIt(String assetGuid, String movementId) {
        Ticket ticket = getAssetNotSendingTicket(assetGuid);
        if (ticket == null) return;
        ticket.setStatus(TicketStatusType.CLOSED);
        ticket.setMovementGuid(movementId);
        CustomRule customRule = rulesServiceBean.getCustomRuleOrAssetNotSendingRule(ticket.getRuleGuid());
        incidentProducer.updatedTicket(new EventTicket(ticket, customRule));
        ticketUpdateEvent.fire(new EventTicket(ticket, customRule));
    }

    private Ticket getAssetNotSendingTicket(String assetGuid) {
        return rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_NOT_SENDING_RULE);
    }

    private Ticket getAssetSendingDespiteParkedTicket(String assetGuid) {
        return rulesDao.getTicketByAssetAndRule(assetGuid, ServiceConstants.ASSET_SENDING_DESPITE_LONG_TERM_PARKED_RULE);
    }
}
