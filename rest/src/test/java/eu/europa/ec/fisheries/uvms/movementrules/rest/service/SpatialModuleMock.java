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

import java.util.ArrayList;
import java.util.List;
import javax.ejb.Stateless;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbBuilder;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import eu.europa.ec.fisheries.uvms.movementrules.rest.service.dto.AreaTransitionsDTO;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.Area;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.AreaExtendedIdentifierType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.AreaType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.AreasByLocationType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.ClosestAreasType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.ClosestLocationsType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.Location;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.LocationType;
import eu.europa.ec.fisheries.uvms.spatial.model.schemas.SpatialEnrichmentRS;

/*
 * Test areas
 *          y (lat)
 *          ^
 *          |
 *     D    |    A
 *          |
 * ---------------------> x (long)
 *          |
 *     C    |    B
 *          |
 */

@Path("spatialSwe/spatialnonsecure/json")
@Stateless
public class SpatialModuleMock {

    private Jsonb jsonb = JsonbBuilder.create();

    @GET
    @Path("getEnrichmentAndTransitions")
    @Consumes(value = {MediaType.APPLICATION_JSON})
    @Produces(value = {MediaType.APPLICATION_JSON})
    public Response getEnrichmentAndTransitions(@QueryParam(value = "firstLongitude") Double firstLongitude, @QueryParam(value = "firstLatitude") Double firstLatitude, @QueryParam(value = "secondLongitude") Double secondLongitude, @QueryParam(value = "secondLatitude") Double secondLatitude) {
        
        
        AreaTransitionsDTO response = new AreaTransitionsDTO();
        
        SpatialEnrichmentRS spatialEnrichmentRS = new SpatialEnrichmentRS();
        populateClosestAreas(spatialEnrichmentRS);
        populateClosestLocations(spatialEnrichmentRS);
        AreasByLocationType currentAreas = getAreas(secondLatitude, secondLongitude);
        spatialEnrichmentRS.setAreasByLocation(currentAreas);
        response.setSpatialEnrichmentRS(spatialEnrichmentRS);

        response.setEnteredAreas(new ArrayList<>());
        response.setExitedAreas(new ArrayList<>());

        ArrayList<AreaExtendedIdentifierType> enteredAreas = new ArrayList<>(currentAreas.getAreas());
        
        if (firstLatitude != null && firstLongitude != null) {
            AreasByLocationType previousAreas = getAreas(firstLatitude, firstLongitude);
            ArrayList<AreaExtendedIdentifierType> exitedAreas = new ArrayList<>(previousAreas.getAreas());
            exitedAreas.removeAll(currentAreas.getAreas());
            response.setExitedAreas(exitedAreas);
            
            enteredAreas.removeAll(previousAreas.getAreas());
        }
        
        response.setEnteredAreas(enteredAreas);
        
        return Response.ok(jsonb.toJson(response)).build();
    }

    private void populateClosestAreas(SpatialEnrichmentRS spatialEnrichmentRS) {
        List<Area> closestAreas = new ArrayList<>();
        Area area = new Area();
        area.setAreaType(AreaType.COUNTRY);
        area.setCode("SWE");
        area.setId("SWE");
        area.setName("Sweden");
        area.setDistance(0d);
        closestAreas.add(area);
        spatialEnrichmentRS.setClosestAreas(new ClosestAreasType(closestAreas));
    }
    
    private void populateClosestLocations(SpatialEnrichmentRS spatialEnrichmentRS) {
        ClosestLocationsType closestLocationsType = new ClosestLocationsType();
        ArrayList<Location> closestLocations = new ArrayList<>();
        Location location = new Location();
        location.setLocationType(LocationType.PORT);
        location.setCode("GOT");
        location.setName("Gothenburg");
        location.setId("PortId");
        location.setDistance(0d);
        closestLocations.add(location);
        closestLocationsType.setClosestLocations(closestLocations);
        spatialEnrichmentRS.setClosestLocations(closestLocationsType);
    }
    
    private AreasByLocationType getAreas(Double latitude, Double longitude) {
        AreasByLocationType areasByLocationType = new AreasByLocationType();
        List<AreaExtendedIdentifierType> areas = new ArrayList<>();
        AreaExtendedIdentifierType area1 = new AreaExtendedIdentifierType();
        String areaSuffix = getTestAreaSuffix(latitude, longitude);
        area1.setId("Area" + areaSuffix);
        area1.setAreaType(AreaType.COUNTRY);
        area1.setCode("Area" + areaSuffix);
        area1.setName("Area" + areaSuffix);
        areas.add(area1);
        AreaExtendedIdentifierType area2 = new AreaExtendedIdentifierType();
        area2.setId("EU");
        area2.setAreaType(AreaType.EEZ);
        area2.setCode("EU");
        area2.setName("Europe");
        areas.add(area2);
        areasByLocationType.setAreas(areas);
        return areasByLocationType;
    }
    
    private String getTestAreaSuffix(Double latitude, Double longitude) {
        if (latitude >= 0 && longitude >= 0) {
            return "A";
        } else if (latitude < 0 && longitude >= 0) {
            return "B";
        } else if (latitude < 0 && longitude < 0) {
            return "C";
        } else {
            return "D";
        }
    }
}
