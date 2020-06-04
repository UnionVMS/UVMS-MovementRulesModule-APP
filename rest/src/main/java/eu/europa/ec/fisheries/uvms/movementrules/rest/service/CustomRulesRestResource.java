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

import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.CustomRuleType;
import eu.europa.ec.fisheries.schema.movementrules.customrule.v1.UpdateSubscriptionType;
import eu.europa.ec.fisheries.schema.movementrules.module.v1.GetCustomRuleListByQueryResponse;
import eu.europa.ec.fisheries.schema.movementrules.search.v1.CustomRuleQuery;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.RulesServiceBean;
import eu.europa.ec.fisheries.uvms.movementrules.service.dto.CustomRuleListResponseDto;
import eu.europa.ec.fisheries.uvms.movementrules.service.entity.CustomRule;
import eu.europa.ec.fisheries.uvms.movementrules.service.mapper.CustomRuleMapper;
import eu.europa.ec.fisheries.uvms.rest.security.RequiresFeature;
import eu.europa.ec.fisheries.uvms.rest.security.UnionVMSFeature;
import eu.europa.ec.fisheries.uvms.user.model.exception.ModelMarshallException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ejb.EJB;
import javax.ejb.Stateless;
import javax.jms.JMSException;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.nio.file.AccessDeniedException;
import java.util.List;
import java.util.UUID;

@Path("/customrules")
@Stateless
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class CustomRulesRestResource {

    private static final Logger LOG = LoggerFactory.getLogger(CustomRulesRestResource.class);

    @EJB
    private RulesServiceBean rulesService;
    @Context
    private ServletContext servletContext;
    @Context
    private HttpServletRequest request;

    @POST
    @RequiresFeature(UnionVMSFeature.manageAlarmRules)
    public Response createCustomRule(CustomRuleType customRule) throws JMSException, ModelMarshallException, AccessDeniedException {
        LOG.info("Create invoked in rest layer");
        if(rulesService.isValid(customRule)) {
            try {
                CustomRule entity = CustomRuleMapper.toCustomRuleEntity(customRule);
                CustomRule created = rulesService.createCustomRule(entity, UnionVMSFeature.manageGlobalAlarmsRules.name(),
                        rulesService.getApplicationName(servletContext));
                CustomRuleType response = CustomRuleMapper.toCustomRuleType(created);
                return Response.ok(response).build();
            } catch (AccessDeniedException e) {
                LOG.error("[ User has no right to create global alarm rules ] {} ", e);
                throw e;
            } catch (Exception e) {
                LOG.error("[ Error when creating. ] {} ", e);
                throw e;
            }
        } else {
            return Response.status(Response.Status.BAD_REQUEST).entity("Custom rule data is not correct").build();
        }
    }

    @GET
    @Path(value = "listAll/{userName}")
    @RequiresFeature(UnionVMSFeature.viewAlarmRules)
    public Response getCustomRulesByUser(@PathParam(value = "userName") final String userName) {
        LOG.info("Get all custom rules invoked in rest layer");
        try {
            List<CustomRule> entityList = rulesService.getCustomRulesByUser(userName);
            List<CustomRuleType> typeList = CustomRuleMapper.toCustomRuleTypeList(entityList);
            return Response.ok(typeList).build();
        } catch (Exception ex) {
            LOG.error("[ Error when getting all custom rules. ]", ex);
            throw ex;
        }
    }

    @POST
    @Path("listByQuery")
    @RequiresFeature(UnionVMSFeature.viewAlarmRules)
    public Response getCustomRulesByQuery(CustomRuleQuery query) {
        LOG.info("Get custom rules by query invoked in rest layer");
        try {
            CustomRuleListResponseDto customRulesListDto = rulesService.getCustomRulesByQuery(query);
            GetCustomRuleListByQueryResponse response = new GetCustomRuleListByQueryResponse();
            response.setTotalNumberOfPages(customRulesListDto.getTotalNumberOfPages());
            response.setCurrentPage(customRulesListDto.getCurrentPage());
            response.getCustomRules().addAll(CustomRuleMapper.toCustomRuleTypeList(customRulesListDto.getCustomRuleList()));
            return Response.ok(response).build();
        } catch (Exception ex) {
            LOG.error("[ Error when getting custom rules by query. ] {} ", ex);
            throw ex;
        }
    }

    @GET
    @Path(value = "{guid}")
    @RequiresFeature(UnionVMSFeature.viewAlarmRules)
    public Response getCustomRuleByGuid(@PathParam(value = "guid") final String guid) {
        LOG.info("Get custom rule by guid invoked in rest layer");
        try {
            CustomRule customRuleByGuid = rulesService.getCustomRuleByGuid(UUID.fromString(guid));
            CustomRuleType response = CustomRuleMapper.toCustomRuleType(customRuleByGuid);
            return Response.ok(response).build();
        } catch (Exception ex) {
            LOG.error("[ Error when getting custom rule by guid. ] {} ", ex);
            throw ex;
        }
    }

    @PUT
    @RequiresFeature(UnionVMSFeature.manageAlarmRules)
    public Response updateCustomRule(final CustomRuleType customRuleType) throws AccessDeniedException, JMSException, ModelMarshallException {
        LOG.info("Update custom rule invoked in rest layer");
        if(rulesService.isValid(customRuleType)) {
            try {
                CustomRule customRule = CustomRuleMapper.toCustomRuleEntity(customRuleType);
                CustomRule updated = rulesService.updateCustomRule(customRule, UnionVMSFeature.manageGlobalAlarmsRules.name(),
                        rulesService.getApplicationName(servletContext));
                CustomRuleType response = CustomRuleMapper.toCustomRuleType(updated);
                return Response.ok(response).build();
            } catch (AccessDeniedException e) {
                LOG.error("Forbidden access", e.getMessage());
                throw e;
            } catch (Exception e) {
                LOG.error("[ Error when updating. ] {} ", e);
                throw e;
            }
        } else {
            return Response.status(Response.Status.BAD_REQUEST).entity("Custom rule data is not correct").build();
        }
    }

    @POST
    @Path("/subscription")
    @RequiresFeature(UnionVMSFeature.manageAlarmRules)
    public Response updateSubscription(UpdateSubscriptionType updateSubscriptionType) {
        LOG.info("Update subscription invoked in rest layer");
        try {
            CustomRule updated = rulesService.updateSubscription(updateSubscriptionType, request.getRemoteUser());
            CustomRuleType response = CustomRuleMapper.toCustomRuleType(updated);
            return Response.ok(response).build();
        } catch (Exception e) {
            LOG.error("[ Error when updating subscription and custom rule. ] {} ", e);
            throw e;
        }
    }

    @DELETE
    @Path("/{guid}")
    @RequiresFeature(UnionVMSFeature.manageAlarmRules)
    public Response deleteCustomRule(@PathParam(value = "guid") final String guid) throws AccessDeniedException {
        LOG.info("Delete custom rule invoked in rest layer");
        try {
            CustomRule deleted = rulesService.deleteCustomRule(guid, request.getRemoteUser(),
                    UnionVMSFeature.manageGlobalAlarmsRules.name(), rulesService.getApplicationName(servletContext));
            CustomRuleType response = CustomRuleMapper.toCustomRuleType(deleted);
            return Response.ok(response).build();
        } catch (AccessDeniedException e) {
            LOG.error("Forbidden access", e.getMessage());
            throw e;
        } catch (Exception e) {
            LOG.error("[ Error when deleting custom rule. ] {} ", e);
            throw e;
        }
    }
}
