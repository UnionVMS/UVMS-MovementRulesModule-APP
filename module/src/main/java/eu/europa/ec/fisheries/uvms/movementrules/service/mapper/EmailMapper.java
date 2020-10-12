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
package eu.europa.ec.fisheries.uvms.movementrules.service.mapper;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import eu.europa.ec.fisheries.uvms.movementrules.model.dto.MovementDetails;

public class EmailMapper {
    
    private static final DateTimeFormatter TIMESTAMP_FORMATTER = DateTimeFormatter
                                                                    .ofPattern("yyyy-MM-dd HH:mm:ss X")
                                                                    .withZone(ZoneId.of("UTC"));

    private EmailMapper() {}
    
    public static String buildSubject(MovementDetails movementDetails) {
        List<String> assetIdentifiers = new ArrayList<>();
        if (movementDetails.getExternalMarking() != null) {
            assetIdentifiers.add(movementDetails.getExternalMarking());
        }
        if (movementDetails.getAssetName() != null) {
            assetIdentifiers.add(movementDetails.getAssetName());
        }
        if (movementDetails.getIrcs() != null) {
            assetIdentifiers.add(movementDetails.getIrcs());
        }
        return String.join(" / ", assetIdentifiers);
    }

    public static String buildBody(String ruleName, MovementDetails movementDetails) {
        StringBuilder sb = new StringBuilder();
        sb.append("<html>")
                .append("Triggered rule: ")
                .append(ruleName)
                .append("<br>")
                .append(TIMESTAMP_FORMATTER.format(movementDetails.getPositionTime()))
                .append("<br><br>")
                .append(buildAssetBodyPart(movementDetails))
                .append(buildPositionBodyPart(movementDetails))
                .append("</html>");

        return sb.toString();
    }

    private static String buildAssetBodyPart(MovementDetails movementDetails) {
        StringBuilder assetBuilder = new StringBuilder();
        assetBuilder.append("<b>Asset:</b>")
                .append("<br>&nbsp;&nbsp;")
                .append("Name: ")
                .append(ignoreNull(movementDetails.getAssetName()))
                .append("<br>&nbsp;&nbsp;")
                .append("XR: ")
                .append(ignoreNull(movementDetails.getExternalMarking()))
                .append("<br>&nbsp;&nbsp;")
                .append("IRCS: ")
                .append(ignoreNull(movementDetails.getIrcs()))
                .append("<br>&nbsp;&nbsp;")
                .append("CFR: ")
                .append(ignoreNull(movementDetails.getCfr()))
                .append("<br>&nbsp;&nbsp;")
                .append("IMO: ")
                .append(ignoreNull(movementDetails.getImo()))
                .append("<br>");

        return assetBuilder.toString();
    }

    private static String buildPositionBodyPart(MovementDetails movementDetails) {
        StringBuilder positionBuilder = new StringBuilder();
        positionBuilder.append("<b>Position report:</b>")
                .append("<br>&nbsp;&nbsp;")
                .append("Timestamp: ")
                .append(TIMESTAMP_FORMATTER.format(movementDetails.getPositionTime()))
                .append("<br>&nbsp;&nbsp;")
                .append("Lat: ")
                .append(getLatitudeString(movementDetails.getLatitude()))
                .append("<br>&nbsp;&nbsp;")
                .append("Lon: ")
                .append(getLongitudeString(movementDetails.getLongitude()))
                .append("<br>&nbsp;&nbsp;")
                .append("DecLat: ")
                .append(ignoreNull(movementDetails.getLatitude(), 6))
                .append("<br>&nbsp;&nbsp;")
                .append("DecLon: ")
                .append(ignoreNull(movementDetails.getLongitude(), 6))
                .append("<br>&nbsp;&nbsp;")
                .append("Status code: ")
                .append(ignoreNull(movementDetails.getStatusCode()))
                .append("<br>&nbsp;&nbsp;")
                .append("Meas. speed: ")
                .append(ignoreNull(movementDetails.getReportedSpeed(), 2))
                .append(" kts")
                .append("<br>&nbsp;&nbsp;")
                .append("Calc. speed: ")
                .append(ignoreNull(movementDetails.getCalculatedSpeed(), 2))
                .append(" kts")
                .append("<br>&nbsp;&nbsp;")
                .append("Meas. course: ")
                .append(ignoreNull(movementDetails.getReportedCourse(), 1))
                .append(" °")
                .append("<br>&nbsp;&nbsp;")
                .append("Calc. course: ")
                .append(ignoreNull(movementDetails.getCalculatedCourse(), 1))
                .append(" °")
                .append("<br>&nbsp;&nbsp;")
                .append("Com channel type: ")
                .append(ignoreNull(movementDetails.getComChannelType()))
                .append("<br>&nbsp;&nbsp;")
                .append("Source: ")
                .append(ignoreNull(movementDetails.getSource()))
                .append("<br>&nbsp;&nbsp;")
                .append("Ocean region: ")
                .append(ignoreNull(movementDetails.getOceanRegion()))
                .append("<br>&nbsp;&nbsp;")
                .append("Movement type: ")
                .append(ignoreNull(movementDetails.getMovementType()))
                .append("<br>&nbsp;&nbsp;");

        positionBuilder.append("Areas:");
        Map<String, List<String>> areas = getAreaMap(movementDetails.getAreaCodes(), movementDetails.getAreaTypes());
        positionBuilder.append("<br>&nbsp;&nbsp;&nbsp;&nbsp;");
        positionBuilder.append("Current Port: ");
        positionBuilder.append(String.join("/", areas.getOrDefault("PORTAREA", Arrays.asList("-"))));
        for (Entry<String, List<String>> entry : areas.entrySet()) {
            positionBuilder.append("<br>&nbsp;&nbsp;&nbsp;&nbsp;")
            .append(entry.getKey())
            .append(" : ");
            positionBuilder.append(String.join("/", entry.getValue()));
        }

        return positionBuilder.toString();
    }
    
    private static String getLatitudeString(Double latitude) {
        String direction = null;
        if (latitude < 0) {
            latitude = -latitude;
            direction = "S";
        } else {
            direction = "N";
        }
        return getCoordString(latitude, direction);
    }

    private static String getLongitudeString(Double longitude) {
        String direction = null;
        if (longitude < 0) {
            longitude = -longitude;
            direction = "W";
        } else {
            direction = "E";
        }
        return getCoordString(longitude, direction);
    }
    
    private static String getCoordString(Double coord, String direction) {
        int deg = (int) Math.floor(coord);
        double min = (coord - deg) * 60;
        
        StringBuilder sb = new StringBuilder();
        sb.append(deg).append('°');
        sb.append(BigDecimal.valueOf(min).setScale(3, RoundingMode.HALF_UP));
        sb.append("'");
        sb.append(direction);
        return sb.toString();
    }
    
    private static String ignoreNull(Double value, int scale) {
        if (value == null) {
            return "-";
        }
        return BigDecimal.valueOf(value).setScale(scale, RoundingMode.HALF_UP).toString();
    }
    
    private static String ignoreNull(String value) {
        if (value == null) {
            return "-";
        }
        return value;
    }
    
    private static Map<String, List<String>> getAreaMap(List<String> codes, List<String> types) {
        Map<String, List<String>> areaMap = new HashMap<>();
        for (int i = 0; i < codes.size(); i++) {
            areaMap.computeIfAbsent(types.get(i), k -> new ArrayList<String>())
                   .add(codes.get(i));
        }
        return areaMap;
    }
}
