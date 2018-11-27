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
package eu.europa.ec.fisheries.uvms.movementrules.service.business;


import java.time.Instant;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

public class MRDateUtils {
    
    private static final String FORMAT = "yyyy-MM-dd HH:mm:ss Z";

    private MRDateUtils() {}
    
    public static Instant stringToDate(String date){
        if (date != null) {
            return ZonedDateTime.parse(date, DateTimeFormatter.ofPattern(FORMAT)).toInstant();   //goes via ZonedDateTime to make sure that it can handle formats other then ISO_INSTANT, for example formats other then 2011-12-03T10:15:30Z and does not cry in pain from a zone
        } else {
            return null;
        }
    }

    public static String dateToString(Instant date) {
        String dateString = null;
        if (date != null) {
            dateString = date.atOffset(ZoneOffset.UTC).format(DateTimeFormatter.ofPattern(FORMAT));
        }
        return dateString;
    }

}