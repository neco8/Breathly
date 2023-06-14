import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import sassPlugin from "vite-plugin-sass";
import { VitePWA } from "vite-plugin-pwa";

export default defineConfig({
  plugins: [
    elmPlugin(),
    sassPlugin(),
    VitePWA({
      registerType: "autoUpdate",
      manifest: {
        name: "Breathly",
        short_name: "Breathing Practice App",
        icons: [],
        display: "standalone",
      },
    }),
  ],
});
