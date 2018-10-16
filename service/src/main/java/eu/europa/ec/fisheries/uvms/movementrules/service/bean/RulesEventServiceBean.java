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

import javax.ejb.Stateless;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsRequest;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetTicketsAndRulesByMovementsResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.PingResponse;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.SetMovementReportRequest;
import eu.europa.ec.fisheries.uvms.movementrules.model.exception.MovementRulesModelMapperException;
import eu.europa.ec.fisheries.uvms.movementrules.model.mapper.MovementRulesModuleResponseMapper;
import eu.europa.ec.fisheries.uvms.movementrules.service.RulesService;
import eu.europa.ec.fisheries.uvms.movementrules.service.exception.RulesServiceException;

@Stateless
public class RulesEventServiceBean{

    private static final Logger LOG = LoggerFactory.getLogger(RulesEventServiceBean.class);

    @Inject
    private RulesService rulesService;
    
    @Inject
    private MovementReportProcessorBean movementReportBean;

    public PingResponse pingReceived() {
        PingResponse pingResponse = new PingResponse();
        pingResponse.setResponse("pong");
        return pingResponse;
    }

    public void setMovementReportReceived(SetMovementReportRequest request) throws RulesServiceException {

            movementReportBean.setMovementReportReceived(request.getRequest(), request.getType().name(), request.getUsername());
    }

    public GetTicketsAndRulesByMovementsResponse getTicketsAndRulesByMovementsEvent(GetTicketsAndRulesByMovementsRequest request) throws Exception {
        try {
            return rulesService.getTicketsAndRulesByMovements(request.getMovementGuids());
        } catch (RulesServiceException e) {
            LOG.error("[ERROR] Error when fetching tickets and rules by movements {}", e);
            throw new Exception("[ERROR] Error when fetching tickets and rules by movements", e);   //TODO: Make this throw something sane, if at all
        }
    }
}