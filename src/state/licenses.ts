import { createSignal } from "solid-js";

/** Whether the "Open source licenses" dialog is shown. */
export const [licensesOpen, setLicensesOpen] = createSignal(false);
