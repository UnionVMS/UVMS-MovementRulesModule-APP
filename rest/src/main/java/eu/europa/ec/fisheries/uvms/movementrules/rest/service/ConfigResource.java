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

import eu.europa.ec.fisheries.schema.movementrules.ticket.v1.TicketStatusType;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.ConfigServiceBean;
import eu.europa.ec.fisheries.uvms.rest.security.RequiresFeature;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ejb.Stateless;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.HashMap;
import java.util.Map;

@Path("/config")
@Stateless
@RequiresFeature(UnionVMSFeature.viewAlarmRules)
@Consumes(value = {MediaType.APPLICATION_JSON})
@Produces(value = {MediaType.APPLICATION_JSON})
public class ConfigResource {

    private static final Logger LOG = LoggerFactory.getLogger(ConfigResource.class);

    @Inject
    private ConfigServiceBean configService;

    @GET
    @Path(value = "/")
    public Response getConfig() {
        try {
            Map map = new HashMap();
            map.put("CRITERIA", configService.getCriterias());
            map.put("ACTIONS", configService.getActions());
            map.put("LOGIC_OPERATORS", configService.getLogicOperatorType());
            map.put("AVAILABILITY", configService.getAvailability());
            map.put("MOBILETERMINAL_STATUSES", configService.getMobileTerminalStatuses());
            map.put("ASSET_STATUSES", configService.getAssetStatuses());
            return Response.ok(map).build();
        } catch (Exception ex) {
            LOG.error("[ Error when getting config. ] {} ", ex.getMessage());
            throw ex;
        }
    }

    @GET
    @Path(value = "/ticketstatus")
    public Response getTicketStatuses() {
        try {
            return Response.ok(TicketStatusType.values()).build();
        } catch (Exception ex) {
            LOG.error("[ Error when getting ticket statuses. ] {} ", ex.getMessage());
            throw ex;
        }
    }
}
