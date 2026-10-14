# Signing

How to sign Arnold so it installs and opens on macOS and Windows without warnings. Builds are
currently unsigned (macOS is ad-hoc signed). Nothing below is set up yet.

## macOS

Signing and notarizing removes the Gatekeeper warnings completely.

**What to get:** the Apple Developer Program ($99 a year). Enrol as a company so the certificate
shows the company name. From the account, create:

- a **Developer ID Application** certificate, which signs `Arnold.app`;
- a **Developer ID Installer** certificate, which signs the `.pkg` (needed because Arnold ships
  as a pkg, not a dmg);
- an **App Store Connect API key** (Users and Access → Integrations), which Apple uses to
  notarize the builds.

**What changes in the repo:**

1. Delete `"signingIdentity": "-"` from `src-tauri/tauri.conf.json`. That setting is the ad-hoc
   signing.
2. Add GitHub secrets that Tauri reads automatically. With these set, `tauri-action` signs the
   app with the hardened runtime and notarizes it.
   - `APPLE_CERTIFICATE`: the Application .p12 file, base64-encoded
   - `APPLE_CERTIFICATE_PASSWORD`
   - `APPLE_SIGNING_IDENTITY`
   - `APPLE_API_ISSUER`, `APPLE_API_KEY` and `APPLE_API_KEY_PATH`: these enable notarization
3. In `src-tauri/installer/macos/build-pkg.sh`:
   - sign the package with `pkgbuild --sign "Developer ID Installer: …"`;
   - notarize it with `xcrun notarytool submit --wait`;
   - staple the approval to it with `xcrun stapler staple`.

   The Installer certificate also has to be imported into the CI keychain, because Tauri only
   imports the Application one.

Notarization adds a few minutes to each macOS build.

## Windows

Signing removes "Unknown publisher", but SmartScreen can still show "Windows protected your PC"
until the publisher has built up download reputation. EV certificates used to skip this, but
Microsoft ended that in 2024. No product gives a warning-free first release on Windows.

### Recommended: Azure Artifact Signing

Called Trusted Signing until recently.

- **Cost:** $9.99 a month for 5,000 signatures. Signing on every push to `main` fits easily.
- **Who can use it:** organizations in the US, Canada, the EU or the UK. Individuals qualify only
  in the US and Canada.
- **How it works:** Microsoft keeps the key and verifies the company's legal name. No USB token or
  cloud HSM (hosted hardware key store) to handle.
- **Setup:** set `bundle.windows.signCommand` in `src-tauri/tauri.conf.json` to a command that
  signs through the service and takes the file path as `%1`. Tauri then signs both `Arnold.exe`
  and the NSIS/MSI installer. The CI login uses the `AZURE_CLIENT_ID`, `AZURE_TENANT_ID` and
  `AZURE_CLIENT_SECRET` secrets.
- **Check first:** the Tauri docs have no example for this service (they only show Azure Key
  Vault with `relic`), so confirm the exact signing tool works before relying on it.

### Fallback: OV certificate

An OV certificate from a CA such as SSL.com or DigiCert costs roughly $200–400 a year. Since 2023
the private key must stay in hardware, so CI needs the CA's cloud signing service (SSL.com eSigner,
DigiCert KeyLocker) called from `signCommand`.

The `arnold.cmd` and `ace.cmd` shims can't be signed. That doesn't matter, because SmartScreen only
checks files that were downloaded, here the installer.

## Plan

Once the accounts exist and the secrets are in the repo, change `release.yml`,
`tauri.conf.json` and `build-pkg.sh`. Each change should only apply when its secrets exist, so
releases keep working unsigned in the meantime.

## Sources

- [Tauri – Windows Code Signing](https://v2.tauri.app/distribute/sign/windows/)
- [DevClass – Azure Artifact Signing GA (Jan 2026)](https://devclass.com/2026/01/14/code-signing-windows-apps-may-be-easier-and-more-secure-with-new-azure-artifact-service/)
- [Azure Artifact Signing pricing](https://azure.microsoft.com/sv-se/pricing/details/artifact-signing/)
- [OV/EV code signing eligibility and pricing 2026 (codenote.net)](https://codenote.net/ja/posts/ov-ev-code-signing-certificate-pricing-japan-2026/)
