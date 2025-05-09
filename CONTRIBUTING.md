# Contributing to SAP TRM Tutorial

Thank you for your interest in contributing to the SAP Treasury and Risk Management Tutorial! This document provides guidelines and steps for contributing.

## Getting Started

1. Fork the repository
2. Clone your fork locally
3. Create a new branch for your work
4. Make your changes
5. Push to your fork
6. Submit a Pull Request

## Development Guidelines

### SAP RAP Conventions

All code must follow SAP RAP development guidelines as documented in [rap-rules.md](rap-rules.md):

- Use proper naming conventions (Z or Y namespace)
- Include proper documentation and comments
- Follow CDS view patterns
- Implement required features
- Add appropriate test cases

### File Naming

- ABAP files must follow SAP naming conventions
- Use meaningful and consistent suffixes (_R_, _I_, _C_)
- Test classes should be named appropriately (e.g., ltcl_*)

### Testing

All new features or bug fixes must include:

1. Unit tests using `if_osql_test_environment`
2. Test data setup and cleanup
3. Both positive and negative test scenarios
4. Documentation of test cases

### Documentation

When adding or modifying features:

1. Update relevant documentation
2. Add inline code comments
3. Update README.md if needed
4. Update rap-rules.md for new patterns

### Pull Request Process

1. Ensure your code follows all guidelines
2. Update documentation as needed
3. Add/update test cases
4. Fill out the pull request template completely
5. Reference any related issues

### Commit Messages

Follow the conventional commits specification:

- feat: New feature
- fix: Bug fix
- docs: Documentation changes
- test: Adding or updating tests
- chore: Maintenance tasks

Example: `feat: Add position validation for dates`

## Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Help others learn and grow
- Follow SAP development best practices

## Questions?

If you have questions about contributing, please:

1. Check existing documentation
2. Review closed Pull Requests for examples
3. Open an issue for clarification

Thank you for contributing!