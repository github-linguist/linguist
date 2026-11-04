import { Avatar, AvatarFallback, AvatarImage } from '@repo/ui/components/avatar';
import { Card, CardContent } from '@repo/ui/components/card';
import { Link } from '@tanstack/react-router';

import { contentIcon } from '#/content/model/kind';
import type { RecentEntry } from '#/home/functions';
import { timeAgo } from '#/time';

type RecentEntriesProps = {
  entries: readonly RecentEntry[];
  /** Whether the organization has any collections or maps to write into. */
  hasTypes: boolean;
};

/** The entries touched most recently, newest first, as cards that open the editor. */
export function RecentEntries(props: RecentEntriesProps) @{
  <section aria-labelledby="recent-entries-heading" className="flex flex-col gap-3">
    <h2 id="recent-entries-heading" className="text-sm font-medium">{"Recently edited"}</h2>
    @if (props.entries.length === 0) {
      <Card size="sm">
        <CardContent>
          <p className="max-w-md text-sm text-pretty text-muted-foreground">
            @if (props.hasTypes) {
              {"Nothing has been written yet. Create an entry and it appears here."}
            } @else {
              {"Create a collection or map with the sidebar's New button, and recent edits appear here."}
            }
          </p>
        </CardContent>
      </Card>
    } @else {
      <div className="@container">
        <ul role="list" className="grid grid-cols-1 gap-3 @xl:grid-cols-2 @4xl:grid-cols-3">
          @for (const entry of props.entries; key entry.id) {
            <EntryCard entry={entry} />
          }
        </ul>
      </div>
    }
  </section>
}

function EntryCard({ entry }: { entry: RecentEntry }) @{
  const TypeIcon = contentIcon(entry.type.kind);

  <li>
    <Link
      to={entry.type.kind === 'map' ? '/app/m/$map' : '/app/c/$collection/$entry'}
      params={
        entry.type.kind === 'map'
          ? { map: entry.type.slug }
          : { collection: entry.type.slug, entry: entry.id }
      }
      className="block h-full rounded-xl focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-ring"
    >
      <Card size="sm" className="h-full gap-2.5 hover:bg-muted/50">
        <CardContent className="flex items-center gap-1.5 text-xs text-muted-foreground">
          <TypeIcon aria-hidden weight="duotone" className="size-3.5 shrink-0" />
          <span className="min-w-0 truncate">{entry.type.name}</span>
          <span className="ml-auto shrink-0 tabular-nums">{timeAgo(entry.updatedAt)}</span>
        </CardContent>
        <CardContent>
          <p className="truncate text-sm font-medium">{entry.title}</p>
        </CardContent>
        <CardContent className="flex min-w-0 items-center gap-1.5">
          @if (entry.editor === null) {
            <p className="truncate text-xs text-muted-foreground">
              {`Created ${timeAgo(entry.createdAt)}`}
            </p>
          } @else {
            <>
            <Avatar size="sm" className="size-4">
              @if (entry.editor.image !== null) {
                <AvatarImage src={entry.editor.image} alt="" />
              }
              <AvatarFallback className="text-[0.5rem]">
                {entry.editor.name.slice(0, 1).toUpperCase()}
              </AvatarFallback>
            </Avatar>
            <p className="truncate text-xs text-muted-foreground">
              {`Edited by ${entry.editor.name}`}
            </p>
            </>
          }
        </CardContent>
      </Card>
    </Link>
  </li>
}
