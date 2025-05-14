# Treasury Position Monitor – Data Model & Development Plan

## Entities
- **Position**: Main entity, represents a treasury position (e.g., account, investment, loan)
- **Cashflow**: Linked to Position, represents individual cash movements
- **Instrument**: Linked to Position (and optionally Cashflow), represents the financial instrument

## Associations
- Position → Cashflow (1-to-many)
- Position → Instrument (1-to-1 or 1-to-many)
- Cashflow → Position (many-to-1)
- Cashflow → Instrument (optional, 1-to-1)

## Development Steps
1. Create basic CDS views for each entity, with direct table mapping and associations.
2. Attach behavior definitions to Position and Cashflow.
3. Add composite/consumption views for UI as needed.
4. Organize all files under the folder structure:
   - `tests/treasury position monitor/src`
   - `tests/treasury position monitor/config`
5. Move any risk management files to their corresponding folders for consistency.

## Notes
- Start with the Position basic view, then proceed with Cashflow and Instrument.
- Refactor or move any existing files as needed for organization.
- This approach ensures extensibility and a clean, maintainable structure.