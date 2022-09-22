import { defineConfig } from 'vite';
import solidPlugin from 'vite-plugin-solid';

export default defineConfig({
  plugins: [solidPlugin()],
  server: {
    port: 3000,
  },
  root: './',
  publicDir: 'assets',
  build: {
    target: 'esnext',
  },
});
