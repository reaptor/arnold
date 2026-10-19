# Signing

How to sign Arnold so it installs and opens on macOS and Windows without warnings. Builds are
currently unsigned (macOS is ad-hoc signed). Windows signing is wired into `release.yml` and
turns on once its secrets exist; macOS signing isn't set up yet.

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
  cloud HSM (hosted hardware key store) to handle. Certificates are valid for three days, so every
  signature is timestamped (`http://timestamp.acs.microsoft.com`) to stay valid after that.

**How the release workflow uses it:** the Windows build job's "Set up Windows code signing" step
downloads Microsoft's Artifact Signing dlib (the `Microsoft.ArtifactSigning.Client` NuGet package,
pinned to 1.0.128), finds the newest x64 SignTool from the Windows SDK, writes the dlib's
`metadata.json`, and adds `bundle.windows.signCommand` to `src-tauri/tauri.windows.conf.json`.
Tauri then signs `Arnold.exe`, the NSIS uninstaller and both installers, before it signs the update
packages, so updates install signed files too. The step is skipped while the `AZURE_CLIENT_SECRET`
secret is missing, so releases stay unsigned until then. The config file in the repository doesn't
have `signCommand`, so local builds stay unsigned.

This uses SignTool and the dlib directly, as in Microsoft's SignTool guide, instead of the
`artifact-signing-cli` tool shown in the Tauri docs. That tool is a third-party wrapper around the
same SignTool and dlib, and it would get the client secret.

**Set it up:**

1. In the Azure portal, create an Artifact Signing account and note its region. Complete identity
   validation for the company, then create a **Public Trust** certificate profile.
2. Create an app registration (Microsoft Entra ID → App registrations) and give it a client
   secret.
3. On the certificate profile, give the app registration the **Artifact Signing Certificate
   Profile Signer** role.
4. Add these to the GitHub repository (Settings → Secrets and variables → Actions):

   | Kind | Name | Value |
   | --- | --- | --- |
   | Secret | `AZURE_CLIENT_ID` | the app registration's application (client) ID |
   | Secret | `AZURE_TENANT_ID` | the directory (tenant) ID |
   | Secret | `AZURE_CLIENT_SECRET` | the client secret; adding it turns signing on |
   | Variable | `AZURE_SIGNING_ENDPOINT` | the account region's endpoint, e.g. `https://neu.codesigning.azure.net` for North Europe |
   | Variable | `AZURE_SIGNING_ACCOUNT` | the Artifact Signing account name |
   | Variable | `AZURE_SIGNING_PROFILE` | the certificate profile name |

   The endpoint must match the account's region, or signing fails with 403 Forbidden. The other
   regions' endpoints are listed in
   [Microsoft's SignTool guide](https://learn.microsoft.com/en-us/azure/artifact-signing/how-to-signing-integrations).

5. Run the Release workflow and check the Windows job's signing output. Then check a downloaded
   installer: right-click → Properties → Digital Signatures should show the company name.

Client secrets expire (after two years at most), so renew `AZURE_CLIENT_SECRET` before then.

### Fallback: OV certificate

An OV certificate from a CA such as SSL.com or DigiCert costs roughly $200–400 a year. Since 2023
the private key must stay in hardware, so CI needs the CA's cloud signing service (SSL.com eSigner,
DigiCert KeyLocker) called from `signCommand`.

The `arnold.cmd` and `ace.cmd` shims can't be signed. That doesn't matter, because SmartScreen only
checks files that were downloaded, here the installer.

## Plan

Windows: done in `release.yml`. It turns on once the Azure secrets and variables above exist.

macOS: once the Apple account and secrets exist, change `release.yml`, `tauri.conf.json` and
`build-pkg.sh`. Like Windows signing, each change should only apply when its secrets exist, so
releases keep working unsigned in the meantime.

## Sources

- [Tauri – Windows Code Signing](https://v2.tauri.app/distribute/sign/windows/)
- [Microsoft – Set up SignTool to use Artifact Signing](https://learn.microsoft.com/en-us/azure/artifact-signing/how-to-signing-integrations)
- [DevClass – Azure Artifact Signing GA (Jan 2026)](https://devclass.com/2026/01/14/code-signing-windows-apps-may-be-easier-and-more-secure-with-new-azure-artifact-service/)
- [Azure Artifact Signing pricing](https://azure.microsoft.com/sv-se/pricing/details/artifact-signing/)
- [OV/EV code signing eligibility and pricing 2026 (codenote.net)](https://codenote.net/ja/posts/ov-ev-code-signing-certificate-pricing-japan-2026/)
