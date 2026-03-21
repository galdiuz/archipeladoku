import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
    base: '/archipeladoku/',
    plugins: [elmPlugin({debug: false})],
    worker: {
        format: 'es',
        plugins: () => [elmPlugin()],
    }
});
