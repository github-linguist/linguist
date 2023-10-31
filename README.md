import { createMiddlewareSupabaseClient } from '@supabase/auth-helpers-nextjs';
import type { NextRequest, NextResponse } from 'next/server';

import { getOrRefreshAccessToken } from '@/lib/integrations/github.edge';
import { OAuthToken } from '@/types/types';

export const config = {
  runtime: 'edge',
  maxDuration: 300,
};

const allowedMethods = ['POST'];

const fetchArchiveStream = async (
  owner: string,
  repo: string,
  branch: string,
  accessToken: string | undefined,
) => {
  return fetch(
    `https://api.github.com/repos/${owner}/${repo}/zipball/${branch}`,
    {
      headers: {
        Accept: 'application/vnd.github+json',
        ...(accessToken ? { Authorization: `Bearer ${accessToken}` } : {}),
        'X-GitHub-Api-Version': '2022-11-28',
      },
    },
  );
};

export default async function handler(req: NextRequest, res: NextResponse) {
  if (!req.method || !allowedMethods.includes(req.method)) {
    const headers = new Headers();
    headers.append('Allow', JSON.stringify(allowedMethods));
    return new Response(`Method ${req.method} Not Allowed`, {
      status: 405,
      headers,
    });
  }

  const supabase = createMiddlewareSupabaseClient({ req, res });
  const {
    data: { session },
  } = await supabase.auth.getSession();

  if (!session?.user) {
    return new Response('Forbidden', { status: 403 });
  }

  let accessToken: OAuthToken | undefined = undefined;
  try {
    accessToken = await getOrRefreshAccessToken(session.user.id, supabase);
  } catch {
    // Do nothing
  }

  let archiveStream;
  const params = await req.json();

  const branchToFetch = params.branch || 'main';
  try {
    console.info(
      `Trying to fetch ${branchToFetch} branch of ${params.owner}/${params.repo}...`,
    );

    // Fetch branch. If none is specified, fetch main branch.
    archiveStream = await fetchArchiveStream(
      params.owner,
      params.repo,
      branchToFetch,
      accessToken?.access_token || undefined,
    );
  } catch (e) {
    console.error(`Error fetching ${branchToFetch} branch:`, e);
  }

  if (!archiveStream?.ok && branchToFetch === 'main') {
    // If the previous fetch failed, fallback to the master branch,
    // but only we were trying to fetch the main branch. If another
    // branch was specified explicitly, we should error.
    try {
      console.info('Trying master branch instead...');
      // If main branch doesn't exist, fallback to master
      archiveStream = await fetchArchiveStream(
        params.owner,
        params.repo,
        'master',
        accessToken?.access_token || undefined,
      );
    } catch (e) {
      console.error('Error fetching master branch:', e);
    }
  }

  if (!archiveStream?.ok || !archiveStream.body) {
    return new Response(
      'Failed to download repository. Make sure the "main" or "master" branch is accessible, or specify a branch explicitly.',
      { status: 404 },
    );
  }

  return new Response(archiveStream.body as ReadableStream);
}
