# Architecture Diagram - Treasury Position Monitor

## System Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                     FIORI APPLICATION LAYER                    │
│                                                                │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐       │
│  │  List Report │  │ Object Page  │  │ Value Helps  │       │
│  │   (Manage    │  │  (Position   │  │ (Instrument, │       │
│  │  Positions)  │  │   Details)   │  │  Currency)   │       │
│  └──────────────┘  └──────────────┘  └──────────────┘       │
│         ↓                  ↓                  ↓              │
└────────────────────────────────────────────────────────────────┘
                             ↓
┌────────────────────────────────────────────────────────────────┐
│                    ODATA SERVICE LAYER                         │
│                                                                │
│  Service Binding: ZKKR_UI_POSITION_O4 (OData V4)              │
│  ┌──────────────────────────────────────────────────┐         │
│  │  Service Definition: ZKKR_UI_POSITION            │         │
│  │  ├─ Position  (ZKKR_C_POSITION)                 │         │
│  │  ├─ Cashflow  (ZKKR_C_CASHFLOW)                 │         │
│  │  └─ Instrument (ZKKR_C_INSTRUMENT)              │         │
│  └──────────────────────────────────────────────────┘         │
└────────────────────────────────────────────────────────────────┘
                             ↓
┌────────────────────────────────────────────────────────────────┐
│                   CONSUMPTION LAYER (C_)                       │
│                 [UI Projection + Search]                       │
│                                                                │
│  ┌──────────────────────────────────────────────────┐         │
│  │  ZKKR_C_POSITION                                 │         │
│  │  - Search: position_id, position_descr          │         │
│  │  - Projection on ZKKR_I_POSITION                │         │
│  │  - Enhanced value helps                          │         │
│  │  - Association redirection                       │         │
│  └──────────────────────────────────────────────────┘         │
│                       ↓                                        │
│  ┌──────────────────────────────────────────────────┐         │
│  │  Metadata Extension: zkkr_c_position.ddlx       │         │
│  │  - Selection fields (6)                          │         │
│  │  - Field groups (5)                              │         │
│  │  - Facets (5)                                    │         │
│  │  - Criticality indicators                        │         │
│  │  - Presentation variants                         │         │
│  └──────────────────────────────────────────────────┘         │
│                                                                │
│  ┌──────────────────┐  ┌──────────────────┐                  │
│  │ ZKKR_C_CASHFLOW  │  │ ZKKR_C_INSTRUMENT│                  │
│  │ + UI Metadata    │  │ + UI Metadata    │                  │
│  └──────────────────┘  └──────────────────┘                  │
└────────────────────────────────────────────────────────────────┘
                             ↓
┌────────────────────────────────────────────────────────────────┐
│                  INTERFACE LAYER (I_)                          │
│              [Business Logic + Behavior]                       │
│                                                                │
│  ┌──────────────────────────────────────────────────┐         │
│  │  ZKKR_I_POSITION (Composite View)                │         │
│  │  ├─ Business fields from ZKKR_R_POSITION        │         │
│  │  ├─ Composition: _Cashflow [0..*]               │         │
│  │  ├─ Association: _Instrument [1..1]              │         │
│  │  └─ Virtual field: AmountCriticality             │         │
│  └──────────────────────────────────────────────────┘         │
│                       ↓                                        │
│  ┌──────────────────────────────────────────────────┐         │
│  │  Behavior Definition: ZKKR_I_POSITION.bdef       │         │
│  │  - Draft enabled                                 │         │
│  │  - CRUD operations                               │         │
│  │  - Validations:                                  │         │
│  │    • validateDates                               │         │
│  │    • validateInstrument                          │         │
│  │    • validateCashflowAmount                      │         │
│  │    • validateCashflowDate                        │         │
│  │  - Determinations:                               │         │
│  │    • calculatePositionID                         │         │
│  │    • calculateCashflowID                         │         │
│  │  - Authorization checks                          │         │
│  └──────────────────────────────────────────────────┘         │
│                       ↓                                        │
│  ┌──────────────────────────────────────────────────┐         │
│  │  Behavior Implementation: ZBP_KKR_I_POSITION     │         │
│  │  - validateDates() method                        │         │
│  │  - validateInstrument() method                   │         │
│  │  - calculatePositionID() method                  │         │
│  │  - get_instance_authorizations() method          │         │
│  │  + Unit tests in testclasses                     │         │
│  └──────────────────────────────────────────────────┘         │
│                                                                │
│  ┌──────────────────┐  ┌──────────────────┐                  │
│  │ ZKKR_I_CASHFLOW  │  │ ZKKR_I_INSTRUMENT│                  │
│  │ + Behavior       │  │ (Master Data)    │                  │
│  └──────────────────┘  └──────────────────┘                  │
└────────────────────────────────────────────────────────────────┘
                             ↓
┌────────────────────────────────────────────────────────────────┐
│                      BASIC LAYER (R_)                          │
│                   [Data Access Layer]                          │
│                                                                │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────┐│
│  │ ZKKR_R_POSITION  │  │ ZKKR_R_CASHFLOW  │  │ZKKR_R_INSTRU-││
│  │ - Direct mapping │  │ - Direct mapping │  │MENT          ││
│  │ - Associations   │  │ - Associations   │  │- Master data ││
│  │ - Semantics      │  │ - Semantics      │  │- Semantics   ││
│  └──────────────────┘  └──────────────────┘  └──────────────┘│
│           ↓                     ↓                     ↓       │
└────────────────────────────────────────────────────────────────┘
                             ↓
┌────────────────────────────────────────────────────────────────┐
│                    DATABASE LAYER                              │
│                                                                │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────┐│
│  │   ZTRMPOS        │  │   ZTRMCF         │  │   ZTRMINST   ││
│  │  (Position)      │  │  (Cashflow)      │  │ (Instrument) ││
│  │                  │  │                  │  │              ││
│  │ • position_id PK │  │ • cashflow_id PK │  │ • instrument ││
│  │ • position_descr │  │ • position_id FK │  │   _id PK     ││
│  │ • instrument_id  │  │ • cashflow_type  │  │ • inst_type  ││
│  │   FK             │  │ • cashflow_amt   │  │ • nominal_amt││
│  │ • position_amt   │  │ • currency       │  │ • currency   ││
│  │ • currency       │  │ • value_date     │  │ • int_rate   ││
│  │ • valid_from     │  │ • created_by     │  │ • start_date ││
│  │ • valid_to       │  │ • created_at     │  │ • end_date   ││
│  │ • created_by     │  │ • changed_by     │  │ • issuer     ││
│  │ • created_at     │  │ • changed_at     │  │              ││
│  │ • changed_by     │  │                  │  │              ││
│  │ • changed_at     │  │                  │  │              ││
│  └──────────────────┘  └──────────────────┘  └──────────────┘│
│                                                                │
│  Relationships:                                                │
│  • Position → Cashflow (1:N composition)                      │
│  • Position → Instrument (N:1 association)                    │
└────────────────────────────────────────────────────────────────┘
```

---

## Data Flow

### Read Operation (Display Position)

```
User clicks on Position
        ↓
FIORI List Report / Object Page
        ↓
OData V4 Request
        ↓
Service Binding (ZKKR_UI_POSITION_O4)
        ↓
Consumption View (ZKKR_C_POSITION)
        ↓
Interface View (ZKKR_I_POSITION)
        ↓
Basic View (ZKKR_R_POSITION)
        ↓
Database Table (ZTRMPOS)
        ↓
← Data flows back through all layers ←
        ↓
UI renders with:
  - Field groups
  - Criticality colors
  - Header data points
  - Related cashflows
```

### Write Operation (Create Position)

```
User clicks Create button
        ↓
FIORI enters Edit mode (Draft)
        ↓
User fills in data
        ↓
User clicks Save
        ↓
OData POST request
        ↓
Service Binding
        ↓
Consumption Behavior (ZKKR_C_POSITION)
        ↓
Interface Behavior (ZKKR_I_POSITION)
        ↓
Behavior Handler (ZBP_KKR_I_POSITION)
  ├─ Validations:
  │  ├─ validateDates()
  │  ├─ validateInstrument()
  │  └─ validateCashflowAmount()
  │
  ├─ Determinations:
  │  ├─ calculatePositionID()
  │  └─ calculateCashflowID()
  │
  └─ Authorization Check
        ↓
If valid: Save to Database
        ↓
Activate Draft
        ↓
Update UI with success message
```

---

## Component Interaction

```
┌─────────────────────────────────────────────────────────────┐
│                        USER ACTIONS                         │
│  Search | Filter | Sort | Create | Edit | Delete | Export  │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                     UI LAYER (FIORI)                        │
│                                                             │
│  ┌──────────────┐    ┌──────────────┐   ┌──────────────┐  │
│  │ Filter Bar   │    │ Table/List   │   │ Object Page  │  │
│  │ - Selection  │    │ - Line Items │   │ - Facets     │  │
│  │   Fields     │    │ - Actions    │   │ - Field      │  │
│  │ - Search     │    │ - Criticality│   │   Groups     │  │
│  └──────────────┘    └──────────────┘   └──────────────┘  │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                   METADATA EXTENSIONS                       │
│                                                             │
│  Define UI Behavior:                                        │
│  • What fields to show                                      │
│  • How to group fields                                      │
│  • What colors to use                                       │
│  • Which fields are searchable                              │
│  • Default sorting                                          │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                  CONSUMPTION VIEWS (C_)                     │
│                                                             │
│  Provide UI-optimized data:                                 │
│  • Search fields indexed                                    │
│  • Value helps enhanced                                     │
│  • Associations redirected                                  │
│  • Field projections                                        │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                  INTERFACE VIEWS (I_)                       │
│                                                             │
│  Implement Business Logic:                                  │
│  • Calculate virtual fields                                 │
│  • Define associations                                      │
│  • Apply semantics                                          │
│  • Composition relationships                                │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                   BEHAVIOR LAYER                            │
│                                                             │
│  Control Operations:                                        │
│  • CRUD operations                                          │
│  • Validations                                              │
│  • Determinations                                           │
│  • Authorization                                            │
│  • Draft handling                                           │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                     BASIC VIEWS (R_)                        │
│                                                             │
│  Data Access:                                               │
│  • Direct table mapping                                     │
│  • Basic associations                                       │
│  • Field semantics                                          │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                     DATABASE TABLES                         │
│                                                             │
│  Data Storage:                                              │
│  • ZTRMPOS - Position master data                          │
│  • ZTRMCF - Cashflow transaction data                      │
│  • ZTRMINST - Instrument master data                       │
│  • Draft tables (auto-generated)                           │
└─────────────────────────────────────────────────────────────┘
```

---

## Key Design Patterns

### 1. Three-Tier CDS Architecture

```
Basic Views     → Data access layer (closest to database)
Interface Views → Business logic layer (with behavior)
Consumption Views → UI projection layer (with search)
```

**Benefits**:
- Clear separation of concerns
- Reusability at each layer
- Easy maintenance
- Scalability

### 2. Metadata Extensions Pattern

```
CDS View (Logic)  +  Metadata Extension (UI)  =  Complete Solution
```

**Benefits**:
- Change UI without touching business logic
- Multiple UI variants from same view
- Clean code separation
- Flexibility

### 3. Composition Pattern

```
Position (Parent)
    └─ Cashflow (Child) [0..*]
```

**Benefits**:
- Parent manages child lifecycle
- Automatic cascading operations
- Referential integrity
- Transaction consistency

### 4. Value Help Pattern

```
Field → Enhanced Value Help → Auto-fill Related Fields
```

**Benefits**:
- Better user experience
- Fewer errors
- Faster data entry
- Consistency

### 5. Criticality Pattern

```
Virtual Field (Calculation) → UI Annotation → Visual Indicator
```

**Benefits**:
- No database storage needed
- Real-time calculation
- Visual feedback
- Easy to change logic

---

## Technology Stack

```
┌─────────────────────────────────────────┐
│         Frontend Layer                  │
│  - SAP Fiori Elements                   │
│  - SAPUI5                               │
│  - OData V4 Client                      │
└─────────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│         Service Layer                   │
│  - OData V4 Protocol                    │
│  - Service Definition                   │
│  - Service Binding                      │
└─────────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│         Business Layer                  │
│  - RAP (RESTful ABAP Programming)       │
│  - CDS Views                            │
│  - Behavior Definitions                 │
│  - ABAP Classes                         │
└─────────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│         Data Layer                      │
│  - SAP HANA Database                    │
│  - Database Tables                      │
│  - Draft Tables                         │
└─────────────────────────────────────────┘
```

---

## Security Architecture

```
┌─────────────────────────────────────────┐
│         User Authentication             │
│  - SAP User Management                  │
│  - Single Sign-On (SSO)                 │
└─────────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│         Service Authorization           │
│  - OData Service Authorization          │
│  - Role-based Access                    │
└─────────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│         Business Object Authorization   │
│  - Instance Authorization               │
│  - Field-level Authorization            │
│  - Action Authorization                 │
└─────────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│         Data Authorization              │
│  - Access Control (DCL)                 │
│  - Organizational Level Checks          │
└─────────────────────────────────────────┘
```

---

## Deployment Architecture

```
┌──────────────────────────────────────────────────────────┐
│                   Development System                     │
│  - ABAP Development Tools (Eclipse)                      │
│  - Create/Edit CDS Views, Behavior, etc.                │
│  - Unit Testing                                          │
└──────────────────────────────────────────────────────────┘
                         ↓ (Transport)
┌──────────────────────────────────────────────────────────┐
│                   Quality/Test System                    │
│  - Integration Testing                                   │
│  - User Acceptance Testing                               │
│  - Performance Testing                                   │
└──────────────────────────────────────────────────────────┘
                         ↓ (Transport)
┌──────────────────────────────────────────────────────────┐
│                   Production System                      │
│  - Live Application                                      │
│  - Real User Access                                      │
│  - Monitoring & Support                                  │
└──────────────────────────────────────────────────────────┘
```

---

## Monitoring & Support

```
Application Monitoring:
├─ Performance Metrics
│  ├─ Response time
│  ├─ Database queries
│  └─ Memory usage
│
├─ Error Handling
│  ├─ Application logs
│  ├─ Error messages
│  └─ Stack traces
│
├─ User Analytics
│  ├─ Usage patterns
│  ├─ Feature adoption
│  └─ Performance issues
│
└─ System Health
   ├─ Service availability
   ├─ Data integrity
   └─ Security alerts
```

---

**Created**: November 27, 2025  
**Version**: 1.0  
**Status**: ✅ Complete
