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
        icons: [
          {
            "src": "/assets/192.png",
            "sizes": "192x192",
            "type": "image/png"
          },
          {
            "src": "/assets/256.png",
            "sizes": "256x256",
            "type": "image/png"
          },
          {
            "src": "/assets/512.png",
            "sizes": "512x512",
            "type": "image/png"
          },
          {
            "src": "/assets/512.png",
            "sizes": "512x512",
            "type": "image/png",
            "purpose": "any maskable"
          }
        ]
        display: "standalone",
        background_color: "#ffffff",
      },
    }),
  ],
});
