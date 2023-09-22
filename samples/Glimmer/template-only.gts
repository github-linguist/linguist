import { LinkTo } from '@ember/routing';
import { TOC } from '@ember/component/template-only';
import Resource from 'ember-crate/models/resource';
import HeroIcon from 'ember-heroicons/components/hero-icon';

const formatDate = (date: Date) => {
  const options: Intl.DateTimeFormatOptions = { month: 'short', day: 'numeric', year: 'numeric' };
  return new Intl.DateTimeFormat('en-US', options).format(new Date(date));
};

export const ResourceCard: TOC<{ Args: { resource: Resource } }> = <template>
  <LinkTo
    @route='resources.resource'
    @model={{@resource.slug}}
    class='block max-w-sm rounded-lg border border-gray-200 bg-white drop-shadow-sm hover:bg-slate-50'
  >
    <div class='p-3 md:p-4'>
      <div class='flex gap-2'>
        <h5
          class='text-l mb-2 h-[3em] font-medium tracking-tight text-gray-900 line-clamp-2 xl:text-xl tracking-tight'
        >{{@resource.title}}</h5>
        {{#if @resource.isFeatured}}
          <HeroIcon
            @icon='star'
            @type='solid'
            class='mt-2 h-4 w-4 shrink-0 text-yellow-400'
          />
        {{/if}}
      </div>

      <div
        class='flex flex-col justify-between gap-2 lg:flex-row lg:items-center'
      >
        <div class='flex items-center text-sm text-slate-700'>
          <HeroIcon @icon='clock' @type='outline' class='mr-2 h-4 w-4' />
          <span>{{formatDate @resource.publishDate}}</span>
        </div>
        <div
          class='w-fit grow-0 rounded bg-slate-100 px-2.5 py-0.5 text-xs font-medium text-slate-800'
        >
          {{@resource.type}}
        </div>
      </div>
    </div>
  </LinkTo>
</template>;

export default ResourceCard;