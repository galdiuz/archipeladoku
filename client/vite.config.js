import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
    plugins: [elmPlugin()],
    worker: {
        format: 'es',
        plugins: () => [elmPlugin()],
    }
});
