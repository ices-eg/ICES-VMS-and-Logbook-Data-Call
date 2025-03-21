**TACSAT2 Format**

| **Type** | **Variable**          | **Code** | **Description**          |
|----------|-----------------------|----------|--------------------------|
| Vessel   | Vessel ID             | VE_REF   | 20-character text string |
| Sighting | Latitude              | SI_LATI  | Decimal degrees          |
|          | Longitude             | SI_LONG  | Decimal degrees          |
|          | Date                  | SI_DATE  | DD/MM/YYYY               |
|          | Time                  | SI_TIME  | HH:MM (UTC)              |
|          | Instantaneous speed   | SI_SP    | Knots                    |
|          | Instantaneous heading | SI_HE    | Degrees                  |

**EFLALO2 Format**

| Type          | Variable                                                    | Code             | Format/Unit                                     |
|---------------|-------------------------------------------------------------|------------------|-------------------------------------------------|
| Vessel        | Vessel ID                                                   | VE_REF           | 20 character string                             |
|               | Fleet                                                       | VE_FLT           | DCF regulation                                  |
|               | Home country                                                | VE_COU           | ISO 3166 – 1 alpha-3 codes.                     |
|               | Vessel length                                               | VE_LEN           | Oal (m)                                         |
|               | Vessel power                                                | VE_KW            | kW                                              |
|               | Tonnage                                                     | VE_TON           | GT *(optional)*                                 |
| Fishing trip  | Fishing trip reference number                               | FT_REF           | 20 character string                             |
|               | Departure country                                           | FT_DCOU          | ISO 3166 – 1 alpha-3 codes.                     |
|               | Departure harbour                                           | FT_DHAR          | International harbour codes. (UN LOCODE)        |
|               | Departure date                                              | FT_DDAT          | DD/MM/YYYY                                      |
|               | Departure time                                              | FT_DTIME         | HH:MM                                           |
|               | Landing country                                             | FT_LCOU          | ISO 3166 – 1 alpha-3 codes.                     |
|               | Landing harbour                                             | FT_LHAR          | International harbour codes. (UN LOCODE)        |
|               | Arrival date                                                | FT_LDAT          | DD/MM/YYYY                                      |
|               | Arrival time                                                | FT_LTIME         | HH:MM                                           |
| Log event     | Log event ID                                                | LE_ID            | 25 character string FT_REF_number (1,2,3,etc.)  |
|               |                                                             |                  |                                                 |
|               | Catch date                                                  | LE_CDAT          | DD/MM/YYYY                                      |
|               | Log event start time                                        | LE_STIME         | HH:MM *(Optional)*                              |
|               | Log event end time                                          | LE_ETIME         | HH:MM *(Optional)*                              |
|               | Log event start position latitude                           | LE_SLAT          | Decimal degrees *(Optional)*                    |
|               | Log event start position longitude                          | LE_SLON          | Decimal degrees *(Optional)*                    |
|               | Log event end position latitude                             | LE_ELAT          | Decimal degrees *(Optional)*                    |
|               | Log event end position longitude                            | LE_ELON          | Decimal degrees *(Optional)*                    |
|               | Gear                                                        | LE_GEAR          | 3 character string. DCF metiér level 4          |
|               | Mesh size                                                   | LE_MSZ           | mm stretched mesh                               |
|               | ICES rectangle                                              | LE_RECT          | 37F5, NA=unallocated                            |
|               | ICES division                                               | LE_DIV           | 10 character string (see codes in annex 1)      |
|               | Fishing activity (métier)                                   | LE_MET           | Filled in as output from Lot2 tool              |
|               | Landing weight estimate of species SP1 (FAO species codes)  | LE_KG_\<SP1\>    | Kg                                              |
|               | Landing value of species SP1 (FAO species codes)            | LE_EURO_\<SP1\>  | EURO                                            |
|               | Landing weight estimate of species SPn (FAO species codes)  | LE_KG_\<SPn\>    | Kg                                              |
|               | Landing value of species SPn (FAO species codes)            | LE_EURO_\<SPn\>  | EURO                                            |
