import { spawnSync } from 'child_process';
import type {
    SpawnSyncReturns,
    SpawnSyncOptionsWithStringEncoding,
} from 'child_process';

export interface PrologResult {
    stdout: string;
    stderr: string;
    timedOut: boolean;
    overflowed: boolean;
}

export function callSWIProlog(
    file: string,
    timeoutMs: number = 2000
): PrologResult {
    const opts: SpawnSyncOptionsWithStringEncoding = {
        encoding: 'utf8',
        stdio: 'pipe',
        timeout: timeoutMs,
        // if SIGTERM is ignored, escalate immediately to SIGKILL
        killSignal: 'SIGKILL',
    };

    const result: SpawnSyncReturns<string> = spawnSync(
        'swipl',
        ['-q', '-s', file, '-t', 'halt'],
        opts
    );

    const stdout = result.stdout ?? '';
    const stderr = result.stderr ?? '';
    const err = result.error as NodeJS.ErrnoException | undefined;

    const timedOut = err?.code === 'ETIMEDOUT';
    const overflowed = err?.code === 'ENOBUFS';

    if (err && !timedOut && !overflowed) {
        throw new Error(`Failed to execute swipl: ${err.message}`);
    }
    if (!timedOut && !overflowed && result.status !== 0) {
        throw new Error(`Prolog exited with code ${result.status}: ${stderr.trim()}`);
    }
    return { stdout, stderr, timedOut, overflowed };
}
