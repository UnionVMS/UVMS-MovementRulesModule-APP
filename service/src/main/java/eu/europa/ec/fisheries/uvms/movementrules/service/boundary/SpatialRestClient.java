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
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.ejb.Stateless;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.Area;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.AreaExtendedIdentifierType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.AreaType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.Location;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.LocationType;

@Stateless
public class SpatialRestClient {

    private WebTarget webTarget;

    @Resource(name = "java:global/spatial_endpoint")
    private String spatialEndpoint;
    
    @PostConstruct
    public void initClient() {
        String url = spatialEndpoint + "/spatialnonsecure/json/";
        webTarget = ClientBuilder.newBuilder()
                .connectTimeout(30, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .build()
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
        return webTarget
                .path("getEnrichmentAndTransitions")
                .queryParam("firstLongitude", previousLongitude)
                .queryParam("firstLatitude", previousLatitude)
                .queryParam("secondLongitude", longitude)
                .queryParam("secondLatitude", latitude)
                .request(MediaType.APPLICATION_JSON)
                .get(AreaTransitionsDTO.class);
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
