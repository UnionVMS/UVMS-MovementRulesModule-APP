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
package eu.europa.ec.fisheries.uvms.movementrules.service.boundary;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ContextResolver;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationModule;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.Area;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.AreaExtendedIdentifierType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.AreaType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.Location;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.LocationType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.PointType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.SpatialEnrichmentRQ;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.SpatialEnrichmentRS;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.SpatialModuleMethod;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.UnitType;

@Stateless
public class SpatialRestClient {

    private WebTarget webTarget;

    @Resource(name = "java:global/spatial_endpoint")
    private String spatialEndpoint;
    
    @PostConstruct
    public void initClient() {
        String url = spatialEndpoint + "/spatialnonsecure/json/";

        Client client = ClientBuilder.newClient();
        client.register(new ContextResolver<ObjectMapper>() {
            @Override
            public ObjectMapper getContext(Class<?> type) {
                ObjectMapper mapper = new ObjectMapper();
                mapper.registerModule(new JaxbAnnotationModule());
                mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
                return mapper;
            }
        });
        webTarget = client.target(url);
    }
    
    public void populateAreasAndAreaTransitions(MovementDetails movementDetails) {
        SpatialEnrichmentRS enrichmentCurrentPosition = getEnrichment(movementDetails.getLatitude(), movementDetails.getLongitude());
        enrichWithCountryData(enrichmentCurrentPosition.getClosestAreas().getClosestAreas(), AreaType.COUNTRY, movementDetails);
        enrichWithPortData(enrichmentCurrentPosition.getClosestLocations().getClosestLocations(), LocationType.PORT, movementDetails);
        SpatialEnrichmentRS enrichmentPreviousPosition = getEnrichment(movementDetails.getPreviousLongitude(), movementDetails.getPreviousLongitude());
        mapAreasAndAreaTransitions(enrichmentCurrentPosition, enrichmentPreviousPosition, movementDetails);
    }

    private void mapAreasAndAreaTransitions(SpatialEnrichmentRS enrichmentCurrentPosition,
            SpatialEnrichmentRS enrichmentPreviousPosition, MovementDetails movementDetails) {
        movementDetails.setAreaCodes(new ArrayList<>());
        movementDetails.setAreaTypes(new ArrayList<>());
        for (AreaExtendedIdentifierType area : enrichmentCurrentPosition.getAreasByLocation().getAreas()) {
            movementDetails.getAreaCodes().add(area.getCode());
            if (!movementDetails.getAreaTypes().contains(area.getAreaType().value())) {
                movementDetails.getAreaTypes().add(area.getAreaType().value());
            }
        }
        
        movementDetails.setEntAreaCodes(new ArrayList<>());
        movementDetails.setEntAreaTypes(new ArrayList<>());
        movementDetails.setExtAreaCodes(new ArrayList<>());
        movementDetails.setExtAreaTypes(new ArrayList<>());
        if (enrichmentPreviousPosition == null) {
            for (AreaExtendedIdentifierType area : enrichmentCurrentPosition.getAreasByLocation().getAreas()) {
                movementDetails.getEntAreaCodes().add(area.getCode());
                if (!movementDetails.getEntAreaTypes().contains(area.getAreaType().value())) {
                    movementDetails.getEntAreaTypes().add(area.getAreaType().value());
                }
            }
        } else {
            for (AreaExtendedIdentifierType area : enrichmentCurrentPosition.getAreasByLocation().getAreas()) {
                if (!enrichmentPreviousPosition.getAreasByLocation().getAreas().contains(area)) {
                    movementDetails.getEntAreaCodes().add(area.getCode());
                    if (!movementDetails.getEntAreaTypes().contains(area.getAreaType().value())) {
                        movementDetails.getEntAreaTypes().add(area.getAreaType().value());
                    }
                }
            }
            for (AreaExtendedIdentifierType area : enrichmentPreviousPosition.getAreasByLocation().getAreas()) {
                if (!enrichmentCurrentPosition.getAreasByLocation().getAreas().contains(area)) {
                    movementDetails.getExtAreaCodes().add(area.getCode());
                    if (!movementDetails.getExtAreaTypes().contains(area.getAreaType().value())) {
                        movementDetails.getExtAreaTypes().add(area.getAreaType().value());
                    }
                }
            }
        }
    }

    private SpatialEnrichmentRS getEnrichment(Double latitude, Double longitude) {
        if (latitude == null || longitude == null) {
            return null;
        }
        PointType point = new PointType();
        point.setCrs(4326); //this magical int is the World Geodetic System 1984, aka EPSG:4326. See: https://en.wikipedia.org/wiki/World_Geodetic_System or http://spatialreference.org/ref/epsg/wgs-84/
        point.setLatitude(latitude);
        point.setLongitude(longitude);
        List<LocationType> locationTypes = Collections.singletonList(LocationType.PORT);
        List<AreaType> areaTypes = Collections.singletonList(AreaType.COUNTRY);
        SpatialEnrichmentRQ request = mapToCreateSpatialEnrichmentRequest(point, UnitType.NAUTICAL_MILES, locationTypes, areaTypes);
        
        Response response =  webTarget
                .path("getEnrichment")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.json(request), Response.class);

        SpatialEnrichmentRS spatialEnrichments = response.readEntity(new GenericType<SpatialEnrichmentRS>() {});
        response.close();
        return spatialEnrichments;
    }
    
    private SpatialEnrichmentRQ mapToCreateSpatialEnrichmentRequest(PointType point, UnitType unit, List<LocationType> locationTypes, List<AreaType> areaTypes) {
        SpatialEnrichmentRQ request = new SpatialEnrichmentRQ();
        request.setMethod(SpatialModuleMethod.GET_ENRICHMENT);
        request.setPoint(point);
        request.setUnit(unit);
        SpatialEnrichmentRQ.LocationTypes loc = new SpatialEnrichmentRQ.LocationTypes();
        if (locationTypes != null) {
            loc.getLocationTypes().addAll(locationTypes);
        }
        request.setLocationTypes(loc);

        SpatialEnrichmentRQ.AreaTypes area = new SpatialEnrichmentRQ.AreaTypes();
        if (areaTypes != null) {
            area.getAreaTypes().addAll(areaTypes);
        }
        request.setAreaTypes(area);

        return request;
    }
    
    private void enrichWithCountryData(List<Area> locations, AreaType areaType, MovementDetails movementDetails) {
        for (Area location : locations) {
            if (location.getAreaType() != null &&
                    location.getAreaType().equals(areaType)) {
                movementDetails.setClosestCountryCode(location.getCode());
            }
        }
    }
    
    private void enrichWithPortData(List<Location> locations, LocationType type, MovementDetails movementDetails) {
        for (Location location : locations) {
            if (location.getLocationType().equals(type)) {
                movementDetails.setClosestPortCode(location.getCode());
            }
        }
    }
}
