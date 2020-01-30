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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.jaxrs.json.JacksonJaxbJsonProvider;
import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.movementrules.service.bean.CustomRulesEvaluator;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.Area;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.AreaExtendedIdentifierType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.AreaType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.Location;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.LocationType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Stateless
public class SpatialRestClient {

    private static final Logger LOG = LoggerFactory.getLogger(CustomRulesEvaluator.class);

    private WebTarget webTarget;

    private ObjectMapper objectMapper;

    @Resource(name = "java:global/spatial_endpoint")
    private String spatialEndpoint;
    
    @PostConstruct
    public void initClient() {
        String url = spatialEndpoint + "/spatialnonsecure/json/";
        objectMapper = new ObjectMapper();
        objectMapper.setAnnotationIntrospector(new JacksonAnnotationIntrospector());
        webTarget = ClientBuilder.newBuilder()
                .connectTimeout(30, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .build()
                .register(new JacksonJsonProvider(objectMapper, JacksonJsonProvider.BASIC_ANNOTATIONS))
                .target(url);
    }
    
    public void populateAreasAndAreaTransitions(MovementDetails movementDetails) {
        AreaTransitionsDTO enrichmentCurrentPosition = getEnrichmentAndTransitions(movementDetails.getLatitude(), movementDetails.getLongitude(), 
                                                                            movementDetails.getPreviousLatitude(), movementDetails.getPreviousLongitude());
        if (enrichmentCurrentPosition.getSpatialEnrichmentRS().getClosestAreas() != null) {
            enrichWithCountryData(enrichmentCurrentPosition.getSpatialEnrichmentRS().getClosestAreas().getClosestAreas(), AreaType.COUNTRY, movementDetails);
        }
        if (enrichmentCurrentPosition.getSpatialEnrichmentRS().getClosestLocations() != null) {
            enrichWithPortData(enrichmentCurrentPosition.getSpatialEnrichmentRS().getClosestLocations().getClosestLocations(), LocationType.PORT, movementDetails);
        }
        mapAreasAndAreaTransitions(enrichmentCurrentPosition, movementDetails);
    }

    private void mapAreasAndAreaTransitions(AreaTransitionsDTO enrichmentCurrentPosition, MovementDetails movementDetails) {
        movementDetails.setAreaCodes(new ArrayList<>());
        movementDetails.setAreaTypes(new ArrayList<>());
        if (enrichmentCurrentPosition.getSpatialEnrichmentRS().getAreasByLocation() != null) {
            for (AreaExtendedIdentifierType area : enrichmentCurrentPosition.getSpatialEnrichmentRS().getAreasByLocation().getAreas()) {
                movementDetails.getAreaCodes().add(area.getCode());
                movementDetails.getAreaTypes().add(area.getAreaType().value());
            }
        }
        
        movementDetails.setEntAreaCodes(new ArrayList<>());
        movementDetails.setEntAreaTypes(new ArrayList<>());
        movementDetails.setExtAreaCodes(new ArrayList<>());
        movementDetails.setExtAreaTypes(new ArrayList<>());

        for (AreaExtendedIdentifierType area : enrichmentCurrentPosition.getEnteredAreas()) {
            movementDetails.getEntAreaCodes().add(area.getCode());
            if (!movementDetails.getEntAreaTypes().contains(area.getAreaType().value())) {
                movementDetails.getEntAreaTypes().add(area.getAreaType().value());
            }
        }
        
        for (AreaExtendedIdentifierType area : enrichmentCurrentPosition.getExitedAreas()) {
            movementDetails.getExtAreaCodes().add(area.getCode());
            if (!movementDetails.getExtAreaTypes().contains(area.getAreaType().value())) {
                movementDetails.getExtAreaTypes().add(area.getAreaType().value());
            }
        }
    }

    private AreaTransitionsDTO getEnrichmentAndTransitions(Double latitude, Double longitude, 
                                              Double previousLatitude, Double previousLongitude) {
        String json = webTarget
                .path("getEnrichmentAndTransitions")
                .queryParam("firstLongitude", previousLongitude)
                .queryParam("firstLatitude", previousLatitude)
                .queryParam("secondLongitude", longitude)
                .queryParam("secondLatitude", latitude)
                .request(MediaType.APPLICATION_JSON)
                .get(String.class);
        LOG.debug(json);
        try {
            return objectMapper.readValue(json, AreaTransitionsDTO.class);  //bit of an ugly hack but this is only in place until we have replaced jackson.
        } catch (IOException e) {
            LOG.error(e.toString());
            throw new RuntimeException(e);
        }
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
