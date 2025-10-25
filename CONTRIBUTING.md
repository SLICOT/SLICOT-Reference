# SLICOT Contribution Guide

Thank you for considering contributing to the SLICOT project!
We welcome contributions from everyone. Please follow these guidelines to ensure
a smooth contribution process.

## How to Contribute

1. **Fork the Repository**: Start by forking the repository to your GitHub account.

2. **Clone the Repository**: Clone your forked repository to your local machine.

   ```bash
   git clone https://github.com/your-username/SLICOT-Reference.git
   ```

3. **Create a Branch**: Create a new branch for your feature or bug fix.

   ```bash
   git switch -c feature-or-bugfix-name
   ```

4. **Make Changes**: Implement your changes in the codebase and add yourself to the list
   of contributors in the `CONTRIBUTORS.md` file in the root of the project. By doing this
   you accept the conditions from this contribution guide.

5. **Commit Changes**: Commit your changes with a clear and descriptive commit message.

   ```bash
   git commit -m "Description of changes"
   ```

6. **Push Changes**: Push your changes to your forked repository.

   ```bash
   git push origin feature-or-bugfix-name
   ```

7. **Submit a Pull Request**: Go to the [SLICOT Repository](https://github.com/SLICOT/SLICOT-Reference)
   and submit a pull request. Provide a detailed description of your changes and any related issues.

## Code Style Guide

To maintain consistency across the codebase, please adhere to the code style guidelines specified
in the `.editorconfig` file located in the root directory of the repository. This file contains
settings for indentation, line endings, and other code style preferences. Most modern code editors
support `.editorconfig` and will automatically apply these settings. For more details, we refer To
[https://editorconfig.org/](https://editorconfig.org/). Furthermore, we accept Fortran 77 fixed-form code
(not recommended) or F90 free form code.

## Copyright and Licensing

By contributing to this project, you agree to the following terms regarding copyright and licensing:

- **Copyright Assignment**: You agree that the SLICOT team retains the copyright to the
  entire codebase, including your contributions. This means that while you will be acknowledged as
  a contributor, but the copyright of the code remains with the SLICOT team. The SLICOT team is
  represented by the current maintainers as listed in the [CONTRIBUTORS.md](./CONTRIBUTORS.md) file.

- **Right to Re-license**: The SLICOT team reserves the right to re-license the entire
  codebase, including your contributions, under different terms. This could involve changing the
  licensing terms to better suit the project's goals or to comply with legal requirements. This
  includes also commercial licensing or moving from the current BSD license back to the previous GNU GPL.

- **Acknowledgment**: Significant contributions will be acknowledged in the project's documentation
   and/or release notes, giving credit for your work. Additionally, contributors may be listed in
  the [CONTRIBUTORS.md](./CONTRIBUTORS.md) file as a token of appreciation for their efforts.

We appreciate your contributions and look forward to collaborating with you! If you have any
questions or concerns about these terms, please feel free to open an issue or contact us directly.
