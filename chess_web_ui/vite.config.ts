import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  publicDir: 'assets',
  server: {
    port: 3000,

    proxy: {
      '/api': {
        target: 'http://localhost:8080',
        changeOrigin: true,
        secure: false,
        rewrite: (path) => path.replace(/^\/api/, ""),
      },
      '/ws': {
        target: 'ws://localhost:8080',
        changeOrigin: true,
        secure: false,
        ws: true,
      }
    }
  }

})
